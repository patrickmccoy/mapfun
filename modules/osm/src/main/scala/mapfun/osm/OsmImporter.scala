package mapfun.osm

import crosby.binary.Osmformat
import crosby.binary.BinaryParser
import crosby.binary.file.BlockInputStream

import mapfun.graph.*
import mapfun.geo.*

import java.io.{BufferedInputStream, FileInputStream}
import java.util
import scala.jdk.CollectionConverters.*

/** A routable way, reduced to the parts routing cares about. */
private final case class RawWay(
    refs: Array[Long],
    roadClass: RoadClass,
    forward: Boolean,
    backward: Boolean,
    speedMps: Double
)

/** Summary of what an import produced. Printing this is usually the fastest way
  * to notice something went wrong.
  */
final case class ImportStats(
    waysRead: Long,
    waysKept: Long,
    nodesRead: Long,
    junctions: Int,
    edges: Int,
    droppedByScc: Int
):
  override def toString: String =
    f"""OSM import
       |  ways read       : $waysRead%,d
       |  ways routable   : $waysKept%,d
       |  nodes read      : $nodesRead%,d
       |  junctions kept  : $junctions%,d
       |  edges built     : $edges%,d
       |  dropped (not in largest SCC): $droppedByScc%,d""".stripMargin

/** Turns an `.osm.pbf` extract into a [[GeoGraph]] you can route on.
  *
  * Three ideas do most of the work:
  *
  *  1. **Two passes.** OSM files store nodes before ways, but you cannot tell
  *     which nodes matter until you have seen the ways. Rather than holding
  *     every node's coordinates in memory, pass 1 reads only ways and records
  *     which node ids are referenced; pass 2 reads nodes and keeps just those.
  *
  *  2. **Junction contraction.** An OSM way is a polyline with a node at every
  *     bend. Those bends are geometry, not decisions -- there is nowhere to
  *     turn. Keeping only nodes that are shared between ways (or are way
  *     endpoints) and collapsing the runs between them into single weighted
  *     edges typically shrinks the graph by 5-10x, and every removed node is
  *     one the search no longer has to settle.
  *
  *  3. **Largest strongly connected component.** A clipped extract is full of
  *     roads whose continuation was cut off at the boundary. Restricting to the
  *     largest SCC guarantees every remaining node can reach every other, so
  *     routing never fails for reasons that have nothing to do with the query.
  */
object OsmImporter:

  def load(
      pbfPath: String,
      profile: TravelProfile = TravelProfile.Car,
      costModel: CostModel = CostModel.Time,
      restrictToLargestScc: Boolean = true,
      verbose: Boolean = true
  ): (GeoGraph, ImportStats) =

    def log(msg: String): Unit = if verbose then println(s"[osm] $msg")

    // ---- Pass 1: ways ------------------------------------------------------
    // Collect routable ways and count how many of them touch each node. A node
    // touched more than once is a junction; so is the first or last node of any
    // way, because that is where a way can connect to another.
    log(s"pass 1/2: scanning ways from $pbfPath")
    val ways = scala.collection.mutable.ArrayBuffer.empty[RawWay]
    val refCount = LongIntMap(1 << 20)
    val forcedJunction = scala.collection.mutable.HashSet.empty[Long]
    var waysRead = 0L

    runParser(pbfPath) {
      new WayPass(
        profile,
        onWay = { w =>
          ways += w
          var i = 0
          while i < w.refs.length do
            refCount.addTo(w.refs(i), 1)
            i += 1
          forcedJunction += w.refs(0)
          forcedJunction += w.refs(w.refs.length - 1)
        },
        onWayRead = () => waysRead += 1
      )
    }
    log(f"  ${ways.length}%,d routable ways of $waysRead%,d")

    if ways.isEmpty then
      throw new IllegalStateException(
        s"No routable ways found in $pbfPath for profile $profile. " +
          "Is this actually an OSM PBF extract?"
      )

    // ---- Pass 2: node coordinates -----------------------------------------
    log("pass 2/2: reading coordinates for referenced nodes")
    val coords = LongCoordMap(math.max(1 << 16, refCount.size * 2))
    var nodesRead = 0L

    runParser(pbfPath) {
      new NodePass(
        wanted = id => refCount.getOrElse(id, 0) > 0,
        onNode = (id, lat, lon) => coords.put(id, lat, lon),
        onNodeRead = () => nodesRead += 1
      )
    }
    log(f"  kept ${coords.size}%,d of $nodesRead%,d nodes")

    // ---- Build the contracted graph ---------------------------------------
    // A node becomes a graph vertex only if it is a real decision point:
    // shared between ways, or the end of one.
    val isJunction: Long => Boolean =
      id => refCount.getOrElse(id, 0) > 1 || forcedJunction.contains(id)

    val (finalGraph, dropped) = assemble(
      ways = ways.toSeq,
      coords = coords,
      isJunction = isJunction,
      costModel = costModel,
      profile = profile,
      restrictToLargestScc = restrictToLargestScc,
      name = pbfPath.split('/').last,
      log = log
    )

    val stats = ImportStats(
      waysRead = waysRead,
      waysKept = ways.length.toLong,
      nodesRead = nodesRead,
      junctions = finalGraph.numNodes,
      edges = finalGraph.numEdges,
      droppedByScc = dropped
    )
    (finalGraph, stats)

  /** Collapses ways into a junction-only graph.
    *
    * Split out from [[load]] so it can be exercised on hand-built ways without
    * needing a PBF file -- this is where the fiddly cases live: nodes missing
    * from the extract, ways that loop back on themselves, and one-way
    * directions.
    */
  private[osm] def assemble(
      ways: Seq[RawWay],
      coords: LongCoordMap,
      isJunction: Long => Boolean,
      costModel: CostModel,
      profile: TravelProfile,
      restrictToLargestScc: Boolean,
      name: String,
      log: String => Unit = _ => ()
  ): (GeoGraph, Int) =

    val catalog = NodeCatalog.empty[Long]
    val builder = GraphBuilder(1 << 16)
    val positions = scala.collection.mutable.ArrayBuffer.empty[LatLon]

    def vertexFor(osmId: Long): NodeId =
      catalog.idOf(osmId).getOrElse {
        val id = catalog.intern(osmId)
        val packed = coords.getPacked(osmId)
        positions += LatLon(LongCoordMap.unpackLat(packed), LongCoordMap.unpackLon(packed))
        id
      }

    ways.foreach { way =>
      // Walk the way, accumulating geometric length, and emit an edge each time
      // we arrive at a junction.
      var previousJunction = NoNode
      var accumulated = 0.0
      var lastLat = 0.0
      var lastLon = 0.0
      var haveLast = false

      var i = 0
      while i < way.refs.length do
        val ref = way.refs(i)
        val packed = coords.getPacked(ref)
        // A way can reference a node absent from the extract -- very common
        // near the clip boundary. Breaking the way in two is the right answer;
        // inventing a straight line across the gap is not.
        if packed == Long.MinValue then
          previousJunction = NoNode
          accumulated = 0.0
          haveLast = false
        else
          val lat = LongCoordMap.unpackLat(packed)
          val lon = LongCoordMap.unpackLon(packed)
          if haveLast then
            accumulated += Haversine.distance(LatLon(lastLat, lastLon), LatLon(lat, lon))
          lastLat = lat
          lastLon = lon
          haveLast = true

          if isJunction(ref) then
            val here = vertexFor(ref)
            // `previousJunction != here` drops the degenerate self-loop a
            // closed way (a roundabout, a cul-de-sac loop) would otherwise
            // produce when it returns to its own start.
            if previousJunction != NoNode && previousJunction != here && accumulated > 0.0 then
              val cost = costModel match
                case CostModel.Distance => accumulated
                case CostModel.Time     => accumulated / way.speedMps
              if way.forward then builder.addEdge(previousJunction, here, cost)
              if way.backward then builder.addEdge(here, previousJunction, cost)
            previousJunction = here
            accumulated = 0.0
        i += 1
    }

    builder.ensureNodes(positions.length)
    val raw = builder.build()
    log(f"  contracted to ${raw.numNodes}%,d junctions and ${raw.numEdges}%,d edges")

    val metric = costModel match
      case CostModel.Distance => WeightMetric.Metres
      case CostModel.Time     => WeightMetric.Seconds(profile.maxSpeedMps)

    val full = GeoGraph(raw, positions.toIndexedSeq, name, metric)

    if !restrictToLargestScc || raw.numNodes == 0 then (full, 0)
    else
      val scc = mapfun.algo.Tarjan.sccs(raw)
      val keep = scc.largestComponent
      val filtered = full.filter(n => scc.componentOf(n) == keep)
      log(f"  largest SCC keeps ${filtered.numNodes}%,d of ${raw.numNodes}%,d junctions")
      (filtered, raw.numNodes - filtered.numNodes)

  private def runParser(path: String)(parser: => BinaryParser): Unit =
    val in = new BufferedInputStream(new FileInputStream(path), 1 << 16)
    try new BlockInputStream(in, parser).process()
    finally in.close()

/** Pass 1: ways only.
  *
  * `BinaryParser` from the osmpbf library handles the container format --
  * blob framing, zlib inflation, the per-block string table and coordinate
  * granularity. What it does not do is undo the delta encoding, which is where
  * the interesting details live.
  */
private final class WayPass(
    profile: TravelProfile,
    onWay: RawWay => Unit,
    onWayRead: () => Unit
) extends BinaryParser:

  protected def parse(header: Osmformat.HeaderBlock): Unit = ()
  protected def parseNodes(nodes: util.List[Osmformat.Node]): Unit = ()
  protected def parseDense(nodes: Osmformat.DenseNodes): Unit = ()
  protected def parseRelations(rels: util.List[Osmformat.Relation]): Unit = ()
  def complete(): Unit = ()

  protected def parseWays(ways: util.List[Osmformat.Way]): Unit =
    ways.asScala.foreach { way =>
      onWayRead()
      val tags = readTags(way)

      tags.get("highway").flatMap(RoadClass.fromTag) match
        case Some(rc) if profile.allowed.contains(rc) && isAccessible(tags) =>
          // Way refs are delta-encoded: each value is an offset from the
          // previous one, which is what keeps the file small.
          val count = way.getRefsCount
          if count >= 2 then
            val refs = new Array[Long](count)
            var running = 0L
            var i = 0
            while i < count do
              running += way.getRefs(i)
              refs(i) = running
              i += 1

            val (forward, backward) = onewayFlags(tags, profile)
            val speed = profile.speedMps(rc, tags.get("maxspeed").flatMap(parseMaxSpeed))

            onWay(RawWay(refs, rc, forward, backward, speed))
        case _ => ()
    }

  private def readTags(way: Osmformat.Way): Map[String, String] =
    val n = way.getKeysCount
    if n == 0 then Map.empty
    else
      val b = Map.newBuilder[String, String]
      var i = 0
      while i < n do
        b += (getStringById(way.getKeys(i)) -> getStringById(way.getVals(i)))
        i += 1
      b.result()

  /** Honours the common access tags. Far from complete -- OSM access rules are
    * a deep rabbit hole involving conditional restrictions and vehicle
    * hierarchies -- but it catches the cases that produce visibly wrong routes.
    */
  private def isAccessible(tags: Map[String, String]): Boolean =
    val blocked = Set("no", "private")
    val generalOk = !tags.get("access").exists(blocked.contains)
    val modeOk = profile match
      case TravelProfile.Car  => !tags.get("motor_vehicle").exists(blocked.contains)
      case TravelProfile.Foot => !tags.get("foot").exists(blocked.contains)
      case TravelProfile.Bike => !tags.get("bicycle").exists(blocked.contains)
    generalOk && modeOk

  /** Which directions of travel this way permits.
    *
    * `oneway=-1` means the way is one-way *against* its node order, which is a
    * genuine trap: ignore it and you build edges pointing the wrong way down
    * some streets. Roundabouts are implicitly one-way even without the tag.
    */
  private def onewayFlags(tags: Map[String, String], profile: TravelProfile): (Boolean, Boolean) =
    if !profile.respectOneway then (true, true)
    else
      val roundabout = tags.get("junction").contains("roundabout")
      tags.get("oneway") match
        case Some("yes") | Some("true") | Some("1")       => (true, false)
        case Some("-1") | Some("reverse")                 => (false, true)
        case Some("no")                                   => (true, true)
        case _ if roundabout                              => (true, false)
        case _                                            => (true, true)

  /** `maxspeed` is free text in practice: "50", "30 mph", "RU:urban", "walk". */
  private def parseMaxSpeed(raw: String): Option[Double] =
    val trimmed = raw.trim.toLowerCase
    val mph = trimmed.endsWith("mph")
    val numeric = trimmed.replace("mph", "").replace("km/h", "").replace("kph", "").trim
    numeric.toDoubleOption.map { value =>
      val kph = if mph then value * 1.609344 else value
      kph * 1000.0 / 3600.0
    }

/** Pass 2: node coordinates for the ids the first pass asked for. */
private final class NodePass(
    wanted: Long => Boolean,
    onNode: (Long, Double, Double) => Unit,
    onNodeRead: () => Unit
) extends BinaryParser:

  protected def parse(header: Osmformat.HeaderBlock): Unit = ()
  protected def parseWays(ways: util.List[Osmformat.Way]): Unit = ()
  protected def parseRelations(rels: util.List[Osmformat.Relation]): Unit = ()
  def complete(): Unit = ()

  /** The rare uncompressed node encoding. Most files use dense nodes instead. */
  protected def parseNodes(nodes: util.List[Osmformat.Node]): Unit =
    nodes.asScala.foreach { n =>
      onNodeRead()
      if wanted(n.getId) then onNode(n.getId, parseLat(n.getLat), parseLon(n.getLon))
    }

  /** The encoding almost every real file uses.
    *
    * Ids and coordinates are all delta-encoded against the previous node, so
    * they have to be read strictly in order and accumulated -- there is no way
    * to seek to node k. `parseLat`/`parseLon` then apply the block's
    * granularity and offset to turn the running raw values into degrees.
    */
  protected def parseDense(nodes: Osmformat.DenseNodes): Unit =
    var id = 0L
    var lat = 0L
    var lon = 0L
    val count = nodes.getIdCount
    var i = 0
    while i < count do
      id += nodes.getId(i)
      lat += nodes.getLat(i)
      lon += nodes.getLon(i)
      onNodeRead()
      if wanted(id) then onNode(id, parseLat(lat), parseLon(lon))
      i += 1
