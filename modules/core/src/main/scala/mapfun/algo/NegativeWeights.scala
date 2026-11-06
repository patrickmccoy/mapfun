package mapfun.algo

import mapfun.graph.*

/** Bellman-Ford: shortest paths that tolerate negative edge weights, and detect
  * negative cycles.
  *
  * Dijkstra's correctness rests on "the cheapest unsettled node is final",
  * which a negative edge destroys. Bellman-Ford gives that up entirely and
  * instead relaxes *every* edge, `V - 1` times over. Any shortest path has at
  * most `V - 1` edges, and after round `k` every shortest path using at most
  * `k` edges is correct -- so `V - 1` rounds settles them all. O(V * E), which
  * is far worse than Dijkstra, and the price of the extra generality.
  *
  * A relaxation that still succeeds on round `V` proves a negative cycle: no
  * shortest path exists, because you could go round again and pay less.
  *
  * Road networks do not have negative weights. Elevation-aware cost models,
  * currency arbitrage graphs and job-scheduling constraint systems do.
  */
object BellmanFord:

  enum Outcome:
    case Distances(tree: ShortestPathTree)

    /** Nodes lying on, or reachable from, a negative cycle -- their distances
      * are unbounded below and therefore meaningless.
      */
    case NegativeCycle(affected: Set[NodeId], witness: Vector[NodeId])

  def from(graph: Graph, source: NodeId): Outcome =
    val n = graph.numNodes
    val dist = Array.fill(n)(Double.PositiveInfinity)
    val parent = Array.fill(n)(NoNode)
    dist(source) = 0.0

    // Rounds of full-graph relaxation. The early exit matters: on graphs that
    // converge quickly this turns the worst case into something close to linear.
    var round = 0
    var changed = true
    while round < n - 1 && changed do
      changed = false
      var u = 0
      while u < n do
        val du = dist(u)
        if !du.isInfinite then
          var e = graph.edgeBegin(u)
          val end = graph.edgeEnd(u)
          while e < end do
            val v = graph.target(e)
            val candidate = du + graph.weight(e)
            if candidate < dist(v) then
              dist(v) = candidate
              parent(v) = u
              changed = true
            e += 1
        u += 1
      round += 1

    // One extra round. Anything that still improves is downstream of a negative
    // cycle.
    var offender = NoNode
    var u = 0
    while u < n do
      val du = dist(u)
      if !du.isInfinite then
        var e = graph.edgeBegin(u)
        val end = graph.edgeEnd(u)
        while e < end do
          val v = graph.target(e)
          if du + graph.weight(e) < dist(v) then
            offender = v
            parent(v) = u
          e += 1
      u += 1

    if offender == NoNode then
      Outcome.Distances(ShortestPathTree(source, dist, parent, n))
    else
      Outcome.NegativeCycle(reachableFrom(graph, offender), extractCycle(parent, offender, n))

  /** Walks `parent` back far enough to be certainly inside the cycle, then once
    * more around it.
    */
  private def extractCycle(parent: Array[NodeId], start: NodeId, n: Int): Vector[NodeId] =
    var cur = start
    var i = 0
    while i < n && cur != NoNode do
      cur = parent(cur)
      i += 1
    if cur == NoNode then Vector.empty
    else
      val cycle = scala.collection.mutable.ArrayBuffer(cur)
      var walk = parent(cur)
      while walk != cur && walk != NoNode do
        cycle += walk
        walk = parent(walk)
      cycle.reverse.toVector

  private def reachableFrom(graph: Graph, start: NodeId): Set[NodeId] =
    val seen = new Array[Boolean](graph.numNodes)
    val stack = scala.collection.mutable.ArrayDeque(start)
    seen(start) = true
    val out = Set.newBuilder[NodeId]
    while stack.nonEmpty do
      val u = stack.removeLast()
      out += u
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        val v = graph.target(e)
        if !seen(v) then
          seen(v) = true
          stack.append(v)
        e += 1
    out.result()

/** Floyd-Warshall: shortest paths between every pair of nodes.
  *
  * The recurrence is the clearest piece of dynamic programming in graph theory:
  * having allowed intermediate nodes `0..k-1`, admitting `k` can only help by
  * routing through it, so `d(i,j) = min(d(i,j), d(i,k) + d(k,j))`. The `k` loop
  * has to be outermost -- that is what makes the induction work, and swapping
  * the loop order is a classic silent bug.
  *
  * O(V^3) time and O(V^2) memory, so this caps out around a couple of thousand
  * nodes. Included because it is the reference implementation everything else
  * gets checked against, not because you would route a city with it.
  */
object FloydWarshall:

  final case class AllPairs(dist: Array[Array[Weight]], next: Array[Array[NodeId]]):
    def distance(from: NodeId, to: NodeId): Weight = dist(from)(to)

    /** Reconstructs a path by repeatedly asking "what is the next hop?". */
    def path(from: NodeId, to: NodeId): Option[Path] =
      if dist(from)(to).isInfinite then None
      else
        val nodes = scala.collection.mutable.ArrayBuffer(from)
        var cur = from
        while cur != to do
          cur = next(cur)(to)
          if cur == NoNode then return None
          nodes += cur
        Some(Path(nodes.toIndexedSeq, dist(from)(to)))

  def apply(graph: Graph): AllPairs =
    val n = graph.numNodes
    require(n <= 4000, s"Floyd-Warshall allocates an ${n}x${n} matrix; that is too big")

    val dist = Array.fill(n)(Array.fill(n)(Double.PositiveInfinity))
    val next = Array.fill(n)(Array.fill(n)(NoNode))

    var i = 0
    while i < n do
      dist(i)(i) = 0.0
      next(i)(i) = i
      i += 1

    i = 0
    while i < n do
      var e = graph.edgeBegin(i)
      val end = graph.edgeEnd(i)
      while e < end do
        val j = graph.target(e)
        val w = graph.weight(e)
        if w < dist(i)(j) then // keep the cheapest of any parallel edges
          dist(i)(j) = w
          next(i)(j) = j
        e += 1
      i += 1

    var k = 0
    while k < n do
      var a = 0
      while a < n do
        val dak = dist(a)(k)
        if !dak.isInfinite then
          val rowA = dist(a)
          val rowK = dist(k)
          var b = 0
          while b < n do
            val candidate = dak + rowK(b)
            if candidate < rowA(b) then
              rowA(b) = candidate
              next(a)(b) = next(a)(k)
            b += 1
        a += 1
      k += 1

    AllPairs(dist, next)
