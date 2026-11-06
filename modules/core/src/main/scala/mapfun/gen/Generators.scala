package mapfun.gen

import mapfun.graph.*
import mapfun.geo.*
import scala.util.Random

/** Synthetic graphs, so algorithms can be exercised and compared before any
  * OSM data is involved.
  *
  * Every generator lays its nodes out over a real latitude/longitude box (by
  * default a small patch of Rhode Island), for two reasons: the geometric A*
  * heuristics work unchanged, and exported GeoJSON drops onto a real basemap so
  * you can look at what the search did.
  */
object Generators:

  /** Default layout box -- small enough that the flat-Earth approximation is
    * excellent, and somewhere recognisable when rendered.
    */
  val DefaultBox: (LatLon, LatLon) =
    (LatLon(41.72, -71.50), LatLon(41.90, -71.35))

  /** A rectangular street grid, Manhattan-style.
    *
    * The clearest place to start: shortest paths are hand-checkable, and with
    * uniform weights the Manhattan distance heuristic is exactly admissible, so
    * you can watch A* settle a diamond where Dijkstra settles a disc.
    *
    * With `diagonals`, the grid becomes 8-connected and diagonal edges cost
    * sqrt(2) times a straight one -- at which point Manhattan distance
    * *overestimates* and stops being admissible, which is a failure worth
    * reproducing on purpose.
    */
  def grid(
      width: Int,
      height: Int,
      diagonals: Boolean = false,
      box: (LatLon, LatLon) = DefaultBox
  ): GeoGraph =
    require(width > 0 && height > 0, "grid must be non-empty")
    val (sw, ne) = box
    val n = width * height
    val positions = new Array[LatLon](n)

    def id(x: Int, y: Int): NodeId = y * width + x

    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val fx = if width == 1 then 0.0 else x.toDouble / (width - 1)
        val fy = if height == 1 then 0.0 else y.toDouble / (height - 1)
        positions(id(x, y)) = LatLon(
          sw.lat + fy * (ne.lat - sw.lat),
          sw.lon + fx * (ne.lon - sw.lon)
        )
        x += 1
      y += 1

    val b = GraphBuilder(n)
    b.ensureNodes(n)

    // Weight every edge by its true geographic length, so distances mean
    // metres and the haversine heuristic is directly usable.
    def connect(ax: Int, ay: Int, bx: Int, by: Int): Unit =
      val u = id(ax, ay)
      val v = id(bx, by)
      b.addUndirectedEdge(u, v, Haversine.distance(positions(u), positions(v)))

    y = 0
    while y < height do
      var x = 0
      while x < width do
        if x + 1 < width then connect(x, y, x + 1, y)
        if y + 1 < height then connect(x, y, x, y + 1)
        if diagonals then
          if x + 1 < width && y + 1 < height then connect(x, y, x + 1, y + 1)
          if x > 0 && y + 1 < height then connect(x, y, x - 1, y + 1)
        x += 1
      y += 1

    GeoGraph(b.build(), positions.toIndexedSeq, s"grid-${width}x${height}", WeightMetric.Metres)

  /** A grid with a perfect maze carved through it -- exactly one route between
    * any two cells.
    *
    * Randomised-DFS carving. Useful because the unique-path property makes
    * every shortest-path algorithm agree trivially, while the long twisting
    * corridors are close to the worst case for A*: the straight-line heuristic
    * points confidently through walls.
    */
  def maze(width: Int, height: Int, seed: Long = 42L, box: (LatLon, LatLon) = DefaultBox): GeoGraph =
    val base = grid(width, height, diagonals = false, box)
    val rng = Random(seed)
    val n = width * height
    val visited = new Array[Boolean](n)
    val b = GraphBuilder(n)
    b.ensureNodes(n)

    def id(x: Int, y: Int): NodeId = y * width + x
    def xOf(i: NodeId): Int = i % width
    def yOf(i: NodeId): Int = i / width

    val stack = scala.collection.mutable.ArrayDeque(0)
    visited(0) = true
    while stack.nonEmpty do
      val cur = stack.last
      val x = xOf(cur)
      val y = yOf(cur)
      val candidates = scala.collection.mutable.ArrayBuffer.empty[NodeId]
      if x > 0 && !visited(id(x - 1, y)) then candidates += id(x - 1, y)
      if x + 1 < width && !visited(id(x + 1, y)) then candidates += id(x + 1, y)
      if y > 0 && !visited(id(x, y - 1)) then candidates += id(x, y - 1)
      if y + 1 < height && !visited(id(x, y + 1)) then candidates += id(x, y + 1)

      if candidates.isEmpty then stack.removeLast()
      else
        val next = candidates(rng.nextInt(candidates.length))
        visited(next) = true
        b.addUndirectedEdge(
          cur,
          next,
          Haversine.distance(base.positions(cur), base.positions(next))
        )
        stack.append(next)

    GeoGraph(b.build(), base.positions, s"maze-${width}x${height}", WeightMetric.Metres)

  /** Random geometric graph: scatter points, join any pair within `radius`
    * metres.
    *
    * A much better stand-in for a real road network than a uniform random graph
    * -- it is spatially local, roughly planar, and has bounded degree, all
    * properties that routing algorithms lean on heavily.
    */
  def randomGeometric(
      nodes: Int,
      radiusMetres: Double,
      seed: Long = 42L,
      box: (LatLon, LatLon) = DefaultBox
  ): GeoGraph =
    val rng = Random(seed)
    val (sw, ne) = box
    val positions = Array.fill(nodes)(
      LatLon(
        sw.lat + rng.nextDouble() * (ne.lat - sw.lat),
        sw.lon + rng.nextDouble() * (ne.lon - sw.lon)
      )
    )

    val b = GraphBuilder(nodes)
    b.ensureNodes(nodes)

    // Bucket by cell so this is not O(n^2). Any pair within `radius` must fall
    // in the same cell or an adjacent one, given cells at least `radius` across.
    val latSpan = radiusMetres / Geo.MetresPerDegreeLat
    val meanLat = math.toRadians((sw.lat + ne.lat) / 2)
    val lonSpan = latSpan / math.max(0.01, math.cos(meanLat))

    val cells = scala.collection.mutable.HashMap.empty[(Int, Int), scala.collection.mutable.ArrayBuffer[Int]]
    var i = 0
    while i < nodes do
      val key = ((positions(i).lat / latSpan).floor.toInt, (positions(i).lon / lonSpan).floor.toInt)
      cells.getOrElseUpdate(key, scala.collection.mutable.ArrayBuffer.empty) += i
      i += 1

    i = 0
    while i < nodes do
      val key = ((positions(i).lat / latSpan).floor.toInt, (positions(i).lon / lonSpan).floor.toInt)
      var dr = -1
      while dr <= 1 do
        var dc = -1
        while dc <= 1 do
          cells.get((key._1 + dr, key._2 + dc)).foreach { bucket =>
            bucket.foreach { j =>
              // j > i keeps each pair once.
              if j > i then
                val d = Haversine.distance(positions(i), positions(j))
                if d <= radiusMetres then b.addUndirectedEdge(i, j, d)
            }
          }
          dc += 1
        dr += 1
      i += 1

    GeoGraph(b.build(), positions.toIndexedSeq, s"geometric-$nodes", WeightMetric.Metres)

  /** Erdos-Renyi: every ordered pair becomes an edge with probability `p`.
    *
    * Deliberately *not* map-like -- no locality, no planarity, diameter around
    * log n. Included as a contrast case: A* heuristics are worthless here, and
    * seeing that is the point.
    */
  def erdosRenyi(nodes: Int, p: Double, seed: Long = 42L, maxWeight: Double = 100.0): CsrGraph =
    val rng = Random(seed)
    val b = GraphBuilder(nodes)
    b.ensureNodes(nodes)
    var u = 0
    while u < nodes do
      var v = 0
      while v < nodes do
        if u != v && rng.nextDouble() < p then b.addEdge(u, v, rng.nextDouble() * maxWeight)
        v += 1
      u += 1
    b.build()

  /** A directed acyclic graph: edges only ever point from lower to higher ids.
    *
    * For exercising topological sort and confirming cycle detection stays quiet
    * when it should.
    */
  def randomDag(nodes: Int, edgesPerNode: Int, seed: Long = 42L): CsrGraph =
    val rng = Random(seed)
    val b = GraphBuilder(nodes)
    b.ensureNodes(nodes)
    var u = 0
    while u < nodes do
      var k = 0
      while k < edgesPerNode do
        val remaining = nodes - u - 1
        if remaining > 0 then
          val v = u + 1 + rng.nextInt(remaining)
          b.addEdge(u, v, 1.0 + rng.nextInt(20))
        k += 1
      u += 1
    b.build()
