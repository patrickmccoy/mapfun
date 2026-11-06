package mapfun.algo

import mapfun.graph.*

final case class Edge(from: NodeId, to: NodeId, weight: Weight)

/** Minimum spanning tree/forest: the cheapest set of edges connecting
  * everything, with no cycles.
  *
  * Both algorithms below exploit the same *cut property*: for any way of
  * splitting the nodes into two groups, the cheapest edge crossing the split
  * belongs to some MST. Kruskal and Prim are just two schedules for applying
  * that fact -- globally by weight, or by growing one component outward.
  *
  * Not a routing tool. An MST tells you the cheapest way to *connect* a
  * network (cable, pipe, road-building budget); the tree path between two nodes
  * is usually not their shortest path.
  *
  * Both treat the graph as undirected and assume edges appear in both
  * directions, which is what [[mapfun.graph.CsrGraph.undirected]] and the
  * generators produce.
  */
object Kruskal:

  /** Sorts every edge by weight and takes each one that joins two components.
    *
    * O(E log E), dominated by the sort. [[UnionFind]] answers "would this close
    * a cycle?" in effectively constant time, which is the only reason this is
    * fast.
    */
  def minimumSpanningForest(graph: Graph): Vector[Edge] =
    val edges = distinctUndirectedEdges(graph).sortBy(_.weight)
    val uf = UnionFind(graph.numNodes)
    val chosen = Vector.newBuilder[Edge]
    var taken = 0
    var i = 0
    val target = graph.numNodes - 1
    while i < edges.length && taken < target do
      val e = edges(i)
      if uf.union(e.from, e.to) then
        chosen += e
        taken += 1
      i += 1
    chosen.result()

  /** Collects each undirected edge once, keeping `from < to` so a symmetric
    * adjacency does not yield every edge twice. Self-loops are dropped: they
    * can never be in a spanning tree.
    */
  private def distinctUndirectedEdges(graph: Graph): Array[Edge] =
    val buf = scala.collection.mutable.ArrayBuffer.empty[Edge]
    var u = 0
    while u < graph.numNodes do
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        val v = graph.target(e)
        if u < v then buf += Edge(u, v, graph.weight(e))
        e += 1
      u += 1
    buf.toArray

object Prim:

  /** Grows a tree outward from each unvisited node, always taking the cheapest
    * edge leaving the tree.
    *
    * O(E log V) with the indexed heap. Note what the heap key means here: the
    * cost of the single cheapest edge attaching a node to the tree -- *not* an
    * accumulated path cost as in Dijkstra. The two algorithms differ by exactly
    * that one line, which is the neatest way to remember both.
    */
  def minimumSpanningForest(graph: Graph): Vector[Edge] =
    val n = graph.numNodes
    val best = Array.fill(n)(Double.PositiveInfinity)
    val bestFrom = Array.fill(n)(NoNode)
    val inTree = new Array[Boolean](n)
    val queue = IndexedMinHeap(n)
    val chosen = Vector.newBuilder[Edge]

    var root = 0
    while root < n do
      if !inTree(root) then
        best(root) = 0.0
        queue.push(root, 0.0)
        while queue.nonEmpty do
          val u = queue.popMin()
          inTree(u) = true
          if bestFrom(u) != NoNode then chosen += Edge(bestFrom(u), u, best(u))
          var e = graph.edgeBegin(u)
          val end = graph.edgeEnd(u)
          while e < end do
            val v = graph.target(e)
            val w = graph.weight(e)
            if !inTree(v) && w < best(v) then
              best(v) = w
              bestFrom(v) = u
              queue.push(v, w)
            e += 1
      root += 1

    chosen.result()

/** Yen's algorithm: the k shortest loopless paths between two nodes.
  *
  * Genuinely useful for maps -- "give me three route options" is a product
  * requirement, not an academic exercise -- and a good illustration of building
  * a harder algorithm out of an easier one.
  *
  * The idea: having found path `A`, the next-best path must diverge from it
  * somewhere. So for each node along `A` (the *spur node*), forbid the edges
  * that would let a search re-tread any known path's continuation from that
  * point, forbid the nodes already used in the prefix so the result stays
  * loopless, and run a fresh shortest-path search. Every candidate produced
  * goes into a pool; the cheapest becomes the next answer.
  *
  * Cost is roughly k * V Dijkstra runs, so keep k small.
  */
object Yen:

  def shortestPaths(graph: Graph, source: NodeId, target: NodeId, k: Int): Vector[Path] =
    require(k >= 1, "k must be at least 1")

    val first = Dijkstra.shortestPath(graph, source, target).path
    if first.isEmpty then return Vector.empty

    val accepted = scala.collection.mutable.ArrayBuffer(first.get)
    // Ordered by cost, then by node sequence so ties resolve deterministically.
    val candidates = scala.collection.mutable.TreeSet.empty[Path](
      Ordering.by[Path, (Double, String)](p => (p.cost, p.nodes.mkString(",")))
    )

    while accepted.length < k do
      val previous = accepted.last

      var i = 0
      while i < previous.nodes.length - 1 do
        val spurNode = previous.nodes(i)
        val rootNodes = previous.nodes.take(i + 1)

        // Ban the next edge of every accepted path that shares this prefix --
        // otherwise the search just rediscovers a path we already have.
        val bannedEdges = scala.collection.mutable.HashSet.empty[Int]
        accepted.foreach { p =>
          if p.nodes.length > i + 1 && p.sharesPrefixWith(previous, i + 1) then
            val nextNode = p.nodes(i + 1)
            var e = graph.edgeBegin(spurNode)
            val end = graph.edgeEnd(spurNode)
            while e < end do
              if graph.target(e) == nextNode then bannedEdges += e
              e += 1
        }

        // Ban the prefix's own nodes (except the spur) to keep paths loopless.
        val bannedNodes = rootNodes.dropRight(1).toSet

        val constraints = SearchConstraints(
          nodeAllowed = n => !bannedNodes.contains(n),
          edgeAllowed = e => !bannedEdges.contains(e)
        )

        Dijkstra.shortestPath(graph, spurNode, target, constraints).path.foreach { spur =>
          val full = rootNodes.dropRight(1) ++ spur.nodes
          val candidate = Path(full, Path.costOf(full, graph))
          if !accepted.exists(_.nodes == full) then candidates += candidate
        }

        i += 1

      if candidates.isEmpty then
        // No further loopless paths exist; fewer than k is the honest answer.
        return accepted.toVector
      else
        val best = candidates.head
        candidates.remove(best)
        accepted += best

    accepted.toVector
