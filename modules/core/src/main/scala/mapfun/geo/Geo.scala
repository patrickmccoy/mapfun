package mapfun.geo

import mapfun.graph.*
import mapfun.algo.Heuristic

/** A point on the Earth, in degrees. */
final case class LatLon(lat: Double, lon: Double):
  def distanceTo(other: LatLon): Double = Haversine.distance(this, other)
  override def toString: String = f"($lat%.6f, $lon%.6f)"

object Geo:
  /** Mean Earth radius in metres (IUGG). Treating the Earth as a sphere costs
    * about 0.5% against the WGS84 ellipsoid -- irrelevant next to the error
    * from roads not being straight.
    */
  val EarthRadiusMetres: Double = 6371008.8

  val MetresPerDegreeLat: Double = EarthRadiusMetres * math.Pi / 180.0

/** Great-circle distance.
  *
  * The haversine formulation is used rather than the direct spherical law of
  * cosines because the latter loses precision catastrophically for short
  * distances -- and on a road network, almost every distance is short.
  */
object Haversine:

  def distance(a: LatLon, b: LatLon): Double =
    val lat1 = math.toRadians(a.lat)
    val lat2 = math.toRadians(b.lat)
    val dLat = lat2 - lat1
    val dLon = math.toRadians(b.lon - a.lon)

    val sinLat = math.sin(dLat / 2)
    val sinLon = math.sin(dLon / 2)
    val h = sinLat * sinLat + math.cos(lat1) * math.cos(lat2) * sinLon * sinLon
    2 * Geo.EarthRadiusMetres * math.asin(math.min(1.0, math.sqrt(h)))

  /** Equirectangular approximation: treats a small patch of Earth as flat.
    *
    * Several times faster than haversine because it skips the trig, and A*
    * calls its heuristic once per relaxed edge, so this is a real cost.
    *
    * The catch is that it can *overestimate* slightly over long distances,
    * which breaks admissibility and therefore A*'s optimality guarantee. Use it
    * for city-scale graphs, or scale it down a hair, and be aware you traded a
    * proof for speed.
    */
  def approximate(a: LatLon, b: LatLon): Double =
    val meanLat = math.toRadians((a.lat + b.lat) / 2)
    val x = math.toRadians(b.lon - a.lon) * math.cos(meanLat)
    val y = math.toRadians(b.lat - a.lat)
    Geo.EarthRadiusMetres * math.sqrt(x * x + y * y)

/** Web Mercator projection -- what every slippy map tile is drawn in.
  *
  * Included so graphs can be rendered to flat pixel coordinates. Note it is
  * conformal but not equal-area, and it blows up near the poles, hence the
  * latitude clamp.
  */
object Mercator:
  val MaxLatitude: Double = 85.05112878

  /** To normalised `[0, 1]` square coordinates, y increasing southward. */
  def project(p: LatLon): (Double, Double) =
    val lat = math.max(-MaxLatitude, math.min(MaxLatitude, p.lat))
    val x = (p.lon + 180.0) / 360.0
    val sinLat = math.sin(math.toRadians(lat))
    val y = 0.5 - math.log((1 + sinLat) / (1 - sinLat)) / (4 * math.Pi)
    (x, y)

  def unproject(x: Double, y: Double): LatLon =
    val lon = x * 360.0 - 180.0
    val n = math.Pi * (1 - 2 * y)
    val lat = math.toDegrees(math.atan(math.sinh(n)))
    LatLon(lat, lon)

/** A graph whose nodes have positions -- the thing that makes this a *map*
  * rather than an abstract graph, and the precondition for A* having any
  * heuristic to work with.
  */
final case class GeoGraph(
    graph: CsrGraph,
    positions: IndexedSeq[LatLon],
    name: String = "graph",
    /** What the edge weights mean. Getting this wrong is how A* heuristics
      * silently stop being admissible.
      */
    metric: WeightMetric = WeightMetric.Metres
):
  require(
    graph.numNodes == positions.length,
    s"${graph.numNodes} nodes but ${positions.length} positions"
  )

  def position(node: NodeId): LatLon = positions(node)

  def numNodes: Int = graph.numNodes
  def numEdges: Int = graph.numEdges

  /** Straight-line distance to `target`, in whatever unit the edges use.
    *
    * For [[WeightMetric.Metres]] that is just the great-circle distance -- a
    * lower bound on any road route, because roads cannot beat a straight line.
    *
    * For [[WeightMetric.Seconds]] it must be divided by the fastest speed
    * anywhere in the graph. Divide by anything slower and the heuristic starts
    * overestimating, and A* quietly returns non-optimal routes.
    */
  def heuristicTo(target: NodeId): Heuristic =
    val goal = positions(target)
    metric match
      case WeightMetric.Metres =>
        (node: NodeId) => Haversine.distance(positions(node), goal)
      case WeightMetric.Seconds(maxSpeedMps) =>
        (node: NodeId) => Haversine.distance(positions(node), goal) / maxSpeedMps

  /** Taxicab (L1) distance to `target`, in the graph's weight unit.
    *
    * On a grid where movement is axis-aligned, this is the *exact* remaining
    * distance, and A* with it expands almost nothing beyond the path itself.
    * Compare against [[heuristicTo]], whose straight-line estimate is up to
    * sqrt(2) too small on the same graph and therefore barely steers the search
    * at all.
    *
    * The lesson generalises: admissibility only guarantees a correct answer.
    * How *close* the estimate gets to the truth is what decides whether A* is
    * worth running. A heuristic that always returns a valid but feeble lower
    * bound is admissible and useless.
    *
    * Only admissible when the graph really does restrict movement to the two
    * axes -- on a road network with diagonals it overestimates, and A* would
    * start returning non-optimal routes.
    */
  def manhattanHeuristicTo(target: NodeId): Heuristic =
    val goal = positions(target)
    val scale = metric match
      case WeightMetric.Metres           => 1.0
      case WeightMetric.Seconds(maxSpeed) => 1.0 / maxSpeed
    (node: NodeId) =>
      val p = positions(node)
      val meanLat = math.toRadians((p.lat + goal.lat) / 2)
      val dLat = math.abs(p.lat - goal.lat) * Geo.MetresPerDegreeLat
      val dLon = math.abs(p.lon - goal.lon) * Geo.MetresPerDegreeLat * math.cos(meanLat)
      (dLat + dLon) * scale

  /** Total length of a path in metres, measured geometrically. Independent of
    * the edge weights, so it is a useful cross-check on a travel-time route.
    */
  def lengthMetres(path: Path): Double =
    var total = 0.0
    var i = 0
    while i < path.nodes.length - 1 do
      total += Haversine.distance(positions(path.nodes(i)), positions(path.nodes(i + 1)))
      i += 1
    total

  def boundingBox: (LatLon, LatLon) =
    var minLat = Double.MaxValue
    var minLon = Double.MaxValue
    var maxLat = Double.MinValue
    var maxLon = Double.MinValue
    positions.foreach { p =>
      if p.lat < minLat then minLat = p.lat
      if p.lat > maxLat then maxLat = p.lat
      if p.lon < minLon then minLon = p.lon
      if p.lon > maxLon then maxLon = p.lon
    }
    (LatLon(minLat, minLon), LatLon(maxLat, maxLon))

  /** Spatial index for snapping coordinates to graph nodes. Built lazily
    * because not every use needs one.
    */
  lazy val index: GridIndex = GridIndex(positions)

  def nearestNode(point: LatLon): NodeId = index.nearest(point)

  /** Restricts to the nodes for which `keep` holds, carrying positions across. */
  def filter(keep: NodeId => Boolean): GeoGraph =
    val (sub, remap) = graph.subgraph(keep)
    val newPositions = new Array[LatLon](sub.numNodes)
    var i = 0
    while i < remap.length do
      if remap(i) != NoNode then newPositions(remap(i)) = positions(i)
      i += 1
    GeoGraph(sub, newPositions.toIndexedSeq, name, metric)

/** What an edge weight measures. */
enum WeightMetric:
  case Metres

  /** Travel time. Carries the graph's maximum speed in metres/second, which is
    * the only thing that makes an admissible time heuristic possible.
    */
  case Seconds(maxSpeedMps: Double)

/** Uniform grid over the bounding box, for nearest-node lookup.
  *
  * Routing starts from a coordinate, not a node id, so every query needs
  * "which junction is nearest this point?". A linear scan is O(n) per query and
  * becomes the bottleneck once you are doing thousands of them.
  *
  * A grid rather than a k-d tree because road nodes are roughly uniformly dense
  * within a city, which is exactly when bucketing wins -- and it is 40 lines
  * instead of 150. On wildly non-uniform data (a country extract that is mostly
  * empty), a k-d tree would behave better.
  */
final class GridIndex private (
    positions: IndexedSeq[LatLon],
    minLat: Double,
    minLon: Double,
    cellLat: Double,
    cellLon: Double,
    rows: Int,
    cols: Int,
    cellStart: Array[Int],
    cellItems: Array[Int]
):

  private def rowOf(lat: Double): Int =
    math.max(0, math.min(rows - 1, ((lat - minLat) / cellLat).toInt))

  private def colOf(lon: Double): Int =
    math.max(0, math.min(cols - 1, ((lon - minLon) / cellLon).toInt))

  /** Nearest node by great-circle distance.
    *
    * Searches rings of cells outward from the query point and stops once
    * nothing outside the scanned block can beat the best hit so far. A fixed
    * "one more ring" rule is not enough: cells are equal in degrees, not
    * metres, so a cell can be many times wider than it is tall, and the nearest
    * node can sit several rings out along the short axis.
    */
  def nearest(point: LatLon): NodeId =
    if positions.isEmpty then NoNode
    else
      val r0 = rowOf(point.lat)
      val c0 = colOf(point.lon)
      var best = NoNode
      var bestDist = Double.PositiveInfinity
      var ring = 0
      val maxRing = math.max(rows, cols)

      while ring <= maxRing && !(best != NoNode && bestDist <= unscannedLowerBound(point, r0, c0, ring)) do
        var r = r0 - ring
        while r <= r0 + ring do
          if r >= 0 && r < rows then
            var c = c0 - ring
            while c <= c0 + ring do
              // Only the perimeter of this ring; the interior was covered already.
              val onPerimeter =
                ring == 0 || r == r0 - ring || r == r0 + ring || c == c0 - ring || c == c0 + ring
              if c >= 0 && c < cols && onPerimeter then
                val cell = r * cols + c
                var i = cellStart(cell)
                val end = cellStart(cell + 1)
                while i < end do
                  val node = cellItems(i)
                  val d = Haversine.distance(point, positions(node))
                  if d < bestDist then
                    bestDist = d
                    best = node
                  i += 1
              c += 1
          r += 1
        ring += 1

      best

  /** A lower bound, in metres, on the distance from `point` to any node in a
    * cell outside the block of rings `0 until ring` around (r0, c0).
    *
    * Such a node is either beyond the block's north/south edge -- at least that
    * latitude gap away, since a great circle can't beat the meridian -- or
    * beyond its east/west edge. For a longitude gap g, haversine gives
    * hav(d) >= cos(lat1) cos(lat2) hav(g), so using the highest latitude in play
    * keeps the bound safe. A side where the block already reaches the edge of
    * the grid has nothing beyond it.
    */
  private def unscannedLowerBound(point: LatLon, r0: Int, c0: Int, ring: Int): Double =
    val k = ring - 1 // the outermost ring scanned so far
    val inf = Double.PositiveInfinity
    val south = if r0 - k <= 0 then inf else point.lat - (minLat + (r0 - k) * cellLat)
    val north = if r0 + k >= rows - 1 then inf else minLat + (r0 + k + 1) * cellLat - point.lat
    val west  = if c0 - k <= 0 then inf else point.lon - (minLon + (c0 - k) * cellLon)
    val east  = if c0 + k >= cols - 1 then inf else minLon + (c0 + k + 1) * cellLon - point.lon

    val latGap = math.max(0.0, math.min(south, north))
    val lonGap = math.max(0.0, math.min(west, east))
    val latBound = if latGap.isInfinite then inf else math.toRadians(latGap) * Geo.EarthRadiusMetres
    val lonBound =
      if lonGap.isInfinite then inf
      else
        val maxAbsLat = math.max(math.abs(point.lat), math.max(math.abs(minLat), math.abs(minLat + rows * cellLat)))
        val g = math.min(math.Pi, math.toRadians(lonGap))
        2 * Geo.EarthRadiusMetres * math.asin(math.min(1.0, math.cos(math.toRadians(maxAbsLat)) * math.sin(g / 2)))
    math.min(latBound, lonBound)

object GridIndex:

  def apply(positions: IndexedSeq[LatLon]): GridIndex =
    if positions.isEmpty then
      new GridIndex(positions, 0, 0, 1, 1, 1, 1, Array(0, 0), Array.empty)
    else
      var minLat = Double.MaxValue
      var minLon = Double.MaxValue
      var maxLat = Double.MinValue
      var maxLon = Double.MinValue
      positions.foreach { p =>
        if p.lat < minLat then minLat = p.lat
        if p.lat > maxLat then maxLat = p.lat
        if p.lon < minLon then minLon = p.lon
        if p.lon > maxLon then maxLon = p.lon
      }

      // Aim for roughly one node per cell: sqrt(n) cells on a side.
      val side = math.max(1, math.sqrt(positions.length.toDouble).toInt)
      val rows = side
      val cols = side
      val cellLat = math.max(1e-9, (maxLat - minLat) / rows * 1.0000001)
      val cellLon = math.max(1e-9, (maxLon - minLon) / cols * 1.0000001)

      // Same counting-sort bucketing as the CSR graph build.
      val counts = new Array[Int](rows * cols + 1)
      def cellOf(p: LatLon): Int =
        val r = math.max(0, math.min(rows - 1, ((p.lat - minLat) / cellLat).toInt))
        val c = math.max(0, math.min(cols - 1, ((p.lon - minLon) / cellLon).toInt))
        r * cols + c

      positions.foreach(p => counts(cellOf(p) + 1) += 1)
      var i = 0
      while i < rows * cols do
        counts(i + 1) += counts(i)
        i += 1

      val cursor = counts.clone()
      val items = new Array[Int](positions.length)
      i = 0
      while i < positions.length do
        val cell = cellOf(positions(i))
        items(cursor(cell)) = i
        cursor(cell) += 1
        i += 1

      new GridIndex(positions, minLat, minLon, cellLat, cellLon, rows, cols, counts, items)
