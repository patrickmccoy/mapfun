package mapfun.algo

import mapfun.graph.*

/** An estimate of the remaining cost from a node to the search's target.
  *
  * Two properties decide whether A* still returns optimal paths:
  *
  *   - *Admissible*: never overestimates the true remaining cost. Enough for
  *     optimality on its own.
  *   - *Consistent* (a.k.a. monotone): `h(u) <= w(u,v) + h(v)` for every edge.
  *     Stronger, and what lets A* finalise a node the moment it pops it, just
  *     like Dijkstra. Straight-line distance on a road network is consistent,
  *     which is why the geometric heuristic works so cleanly.
  *
  * Get this wrong -- say, by using straight-line *distance* against edges
  * weighted in *seconds* -- and A* will cheerfully return non-optimal routes.
  * That failure mode is quiet and worth provoking deliberately once.
  */
trait Heuristic:
  def estimate(node: NodeId): Weight

object Heuristic:
  /** Estimates zero everywhere. Admissible, consistent, and useless -- A* with
    * this heuristic degenerates into exactly Dijkstra, which is the cleanest
    * way to see that Dijkstra is the special case.
    */
  val zero: Heuristic = _ => 0.0

  def apply(f: NodeId => Weight): Heuristic = (node: NodeId) => f(node)

  /** Scales a heuristic.
    *
    * With `factor > 1` the heuristic may overestimate, A* stops guaranteeing
    * optimality, and in exchange it settles far fewer nodes. This is "weighted
    * A*", and the resulting path is provably within `factor` times optimal --
    * a knob real routing services do turn.
    */
  def weighted(h: Heuristic, factor: Double): Heuristic =
    (node: NodeId) => h.estimate(node) * factor

/** A* search: Dijkstra, but the queue is ordered by `g + h` instead of `g`.
  *
  * `g(n)` is the cost already spent getting to `n`; `h(n)` guesses what remains.
  * Ordering by their sum pulls the frontier towards the target instead of
  * letting it expand as a circle in all directions. On a road network with a
  * good geometric heuristic this routinely cuts settled nodes by an order of
  * magnitude while returning the identical path.
  */
object AStar:

  def search(
      graph: Graph,
      source: NodeId,
      target: NodeId,
      heuristic: Heuristic,
      constraints: SearchConstraints = SearchConstraints.unrestricted
  ): SearchResult =
    val n = graph.numNodes
    val g = Array.fill(n)(Double.PositiveInfinity)
    val parent = Array.fill(n)(NoNode)
    val closed = new Array[Boolean](n)
    val queue = IndexedMinHeap(n)

    var settled = 0

    if constraints.nodeAllowed(source) then
      g(source) = 0.0
      queue.push(source, heuristic.estimate(source))

    var found = false
    while queue.nonEmpty && !found do
      val u = queue.popMin()
      closed(u) = true
      settled += 1

      if u == target then found = true
      else
        val gu = g(u)
        var e = graph.edgeBegin(u)
        val end = graph.edgeEnd(u)
        while e < end do
          val v = graph.target(e)
          if !closed(v) && constraints.edgeAllowed(e) && constraints.nodeAllowed(v) then
            val candidate = gu + graph.weight(e)
            if candidate < g(v) then
              g(v) = candidate
              parent(v) = u
              // The queue key is f = g + h; the recorded cost stays g, since
              // that is the real path cost.
              queue.push(v, candidate + heuristic.estimate(v))
          e += 1

    val path =
      if !found then None
      else
        val buf = scala.collection.mutable.ArrayBuffer.empty[NodeId]
        var cur = target
        while cur != NoNode do
          buf += cur
          cur = parent(cur)
        Some(Path(buf.reverse.toIndexedSeq, g(target)))

    SearchResult(path, settled)

/** Bidirectional Dijkstra: grow one search forward from the source and another
  * backward from the target, and stop when they meet.
  *
  * The payoff is geometric. A one-directional search on a roughly planar graph
  * sweeps a disc of radius `d`; two searches sweep two discs of radius `d/2`,
  * which together cover about half the area. On real road networks the saving
  * is closer to 2-4x.
  *
  * The subtle part is the stopping condition. The first node touched by both
  * searches is usually *not* on the shortest path. The correct rule is to keep
  * going until the sum of the two frontier keys exceeds the best meeting cost
  * found so far, tracking `best` across every relaxation rather than only at
  * settled nodes.
  */
object BidirectionalDijkstra:

  /** `backward` must be `forward.reverse`; it is taken as a parameter so the
    * reversed copy can be built once and reused across many queries.
    */
  def search(forward: Graph, backward: Graph, source: NodeId, target: NodeId): SearchResult =
    require(forward.numNodes == backward.numNodes, "graphs must agree on node count")
    val n = forward.numNodes

    if source == target then return SearchResult(Some(Path(IndexedSeq(source), 0.0)), 0)

    val distF = Array.fill(n)(Double.PositiveInfinity)
    val distB = Array.fill(n)(Double.PositiveInfinity)
    val parentF = Array.fill(n)(NoNode)
    val parentB = Array.fill(n)(NoNode)
    val doneF = new Array[Boolean](n)
    val doneB = new Array[Boolean](n)
    val queueF = IndexedMinHeap(n)
    val queueB = IndexedMinHeap(n)

    distF(source) = 0.0
    distB(target) = 0.0
    queueF.push(source, 0.0)
    queueB.push(target, 0.0)

    var best = Double.PositiveInfinity
    var meeting = NoNode
    var settled = 0
    var searching = true

    // Relaxes one edge in one direction, updating the best known meeting cost
    // whenever the two search trees touch.
    def expand(
        graph: Graph,
        u: NodeId,
        distNear: Array[Double],
        distFar: Array[Double],
        parentNear: Array[NodeId],
        doneNear: Array[Boolean],
        queue: IndexedMinHeap
    ): Unit =
      val du = distNear(u)
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        val v = graph.target(e)
        if !doneNear(v) then
          val candidate = du + graph.weight(e)
          if candidate < distNear(v) then
            distNear(v) = candidate
            parentNear(v) = u
            queue.push(v, candidate)
            // Check the meet here, not only when v is settled: the optimal
            // meeting node may never be settled by both sides.
            val far = distFar(v)
            if !far.isInfinite && candidate + far < best then
              best = candidate + far
              meeting = v
        e += 1

    while searching do
      if queueF.isEmpty || queueB.isEmpty then searching = false
      else if queueF.peekKey + queueB.peekKey >= best then
        // Neither frontier can produce anything cheaper than `best`.
        searching = false
      else if queueF.peekKey <= queueB.peekKey then
        // Advance whichever side is currently cheaper, which keeps the two
        // frontiers balanced.
        val u = queueF.popMin()
        doneF(u) = true
        settled += 1
        expand(forward, u, distF, distB, parentF, doneF, queueF)
      else
        val u = queueB.popMin()
        doneB(u) = true
        settled += 1
        expand(backward, u, distB, distF, parentB, doneB, queueB)

    val path =
      if meeting == NoNode then None
      else
        // Walk back to the source, then forward to the target, joining at the
        // meeting node.
        val head = scala.collection.mutable.ArrayBuffer.empty[NodeId]
        var cur = meeting
        while cur != NoNode do
          head += cur
          cur = parentF(cur)
        val nodes = head.reverse
        cur = parentB(meeting)
        while cur != NoNode do
          nodes += cur
          cur = parentB(cur)
        Some(Path(nodes.toIndexedSeq, best))

    SearchResult(path, settled)
