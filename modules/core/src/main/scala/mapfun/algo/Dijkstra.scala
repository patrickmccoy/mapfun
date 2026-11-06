package mapfun.algo

import mapfun.graph.*

/** Restricts which parts of the graph a search may use.
  *
  * Rather than building a filtered copy of the graph -- which would mean
  * rebuilding the CSR arrays every time -- searches consult these predicates.
  * Yen's k-shortest-paths algorithm needs exactly this: it repeatedly asks for
  * a shortest path "but you may not use these nodes or these edges".
  *
  * `edgeAllowed` takes an *edge slot index*, not a node pair, so banning one of
  * several parallel edges between the same two junctions is expressible.
  */
final case class SearchConstraints(
    nodeAllowed: NodeId => Boolean = _ => true,
    edgeAllowed: Int => Boolean = _ => true
)

object SearchConstraints:
  /** The common case: search the whole graph. */
  val unrestricted: SearchConstraints = SearchConstraints()

/** Dijkstra's algorithm: single-source shortest paths over non-negative edge
  * weights.
  *
  * The invariant that makes it work is that the cheapest unsettled node can
  * never be improved by any route through a more expensive one -- which holds
  * only because no edge subtracts from a path's cost. That is the whole reason
  * negative weights need [[BellmanFord]] instead, and it is worth internalising
  * before reaching for A*, whose correctness argument is a variation on the
  * same theme.
  *
  * Complexity is O((V + E) log V) with the binary [[IndexedMinHeap]] used here.
  */
object Dijkstra:

  /** Grows a shortest-path tree from `source` across the whole graph.
    *
    * If `stopAt` is a real node, the search returns as soon as that node is
    * settled. Distances to nodes not yet settled are then only upper bounds, so
    * read the result through `pathTo(stopAt)` and nothing else. This is a big
    * saving in practice: on a road network, settling one specific target
    * typically finishes long before the frontier has swept the map.
    */
  def from(
      graph: Graph,
      source: NodeId,
      stopAt: NodeId = NoNode,
      constraints: SearchConstraints = SearchConstraints.unrestricted
  ): ShortestPathTree =
    val n = graph.numNodes
    val dist = Array.fill(n)(Double.PositiveInfinity)
    val parent = Array.fill(n)(NoNode)
    val settledFlag = new Array[Boolean](n)
    val queue = IndexedMinHeap(n)

    var settled = 0

    if constraints.nodeAllowed(source) then
      dist(source) = 0.0
      queue.push(source, 0.0)

    var stop = false
    while queue.nonEmpty && !stop do
      val u = queue.popMin()
      // Popping finalises u: nothing left in the queue can reach it cheaper,
      // because every remaining key is >= this one and no edge is negative.
      settledFlag(u) = true
      settled += 1

      if u == stopAt then stop = true
      else
        val du = dist(u)
        var e = graph.edgeBegin(u)
        val end = graph.edgeEnd(u)
        while e < end do
          val v = graph.target(e)
          if !settledFlag(v) && constraints.edgeAllowed(e) && constraints.nodeAllowed(v) then
            val candidate = du + graph.weight(e)
            if candidate < dist(v) then
              dist(v) = candidate
              parent(v) = u
              queue.push(v, candidate)
          e += 1

    ShortestPathTree(source, dist, parent, settled)

  /** Point-to-point convenience wrapper. */
  def shortestPath(
      graph: Graph,
      source: NodeId,
      target: NodeId,
      constraints: SearchConstraints = SearchConstraints.unrestricted
  ): SearchResult =
    val tree = from(graph, source, stopAt = target, constraints = constraints)
    SearchResult(tree.pathTo(target), tree.settled)

/** Breadth-first search viewed as the special case of Dijkstra where every edge
  * costs 1 -- and, because of that, needs no priority queue at all.
  *
  * A plain FIFO queue suffices: nodes are discovered in non-decreasing distance
  * order for free. Worth keeping side by side with Dijkstra, because on unit
  * weights the two must agree, and that equality is one of the sharper tests in
  * the suite.
  */
object Bfs:

  def from(graph: Graph, source: NodeId): ShortestPathTree =
    val n = graph.numNodes
    val dist = Array.fill(n)(Double.PositiveInfinity)
    val parent = Array.fill(n)(NoNode)
    val queue = new Array[Int](n)
    var head = 0
    var tail = 0

    dist(source) = 0.0
    queue(tail) = source
    tail += 1

    while head < tail do
      val u = queue(head)
      head += 1
      val du = dist(u)
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        val v = graph.target(e)
        if dist(v).isInfinite then
          dist(v) = du + 1.0
          parent(v) = u
          queue(tail) = v
          tail += 1
        e += 1

    ShortestPathTree(source, dist, parent, tail)

/** Shortest paths when every edge weighs either 0 or 1.
  *
  * A deque replaces the heap: relaxing across a 0-edge pushes to the front,
  * across a 1-edge to the back, which keeps the deque sorted by construction.
  * O(V + E), no log factor.
  *
  * This is not a toy case. It is how you model "turning onto this road is free
  * but crossing this barrier is not", and it generalises to the dial/bucket
  * queues that real routing engines use when weights are small integers.
  */
object ZeroOneBfs:

  def from(graph: Graph, source: NodeId): ShortestPathTree =
    val n = graph.numNodes
    val dist = Array.fill(n)(Double.PositiveInfinity)
    val parent = Array.fill(n)(NoNode)

    // A growable deque rather than a fixed ring buffer: a node can be re-queued
    // whenever its distance improves, so the number of pushes is bounded by the
    // edge count, not the node count.
    val deque = scala.collection.mutable.ArrayDeque.empty[Int]
    var settled = 0

    dist(source) = 0.0
    deque.append(source)

    while deque.nonEmpty do
      val u = deque.removeHead()
      settled += 1
      val du = dist(u)
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        val v = graph.target(e)
        val w = graph.weight(e)
        require(w == 0.0 || w == 1.0, s"0-1 BFS requires weights in {0, 1}, saw $w")
        val candidate = du + w
        if candidate < dist(v) then
          dist(v) = candidate
          parent(v) = u
          if w == 0.0 then deque.prepend(v) else deque.append(v)
        e += 1

    ShortestPathTree(source, dist, parent, settled)
