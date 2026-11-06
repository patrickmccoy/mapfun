package mapfun.graph

import scala.collection.mutable

/** A route: the nodes visited in order, and what it cost to visit them. */
final case class Path(nodes: IndexedSeq[NodeId], cost: Weight):
  def isEmpty: Boolean = nodes.isEmpty
  def source: NodeId   = nodes.head
  def target: NodeId   = nodes.last

  /** Edges traversed. One fewer than the number of nodes. */
  def hops: Int = math.max(0, nodes.length - 1)

  /** The first `n` edges of this path, with the cost of that prefix.
    *
    * Yen's k-shortest-paths algorithm is built out of these: it repeatedly
    * takes a prefix of an already-found path and searches for a different way
    * to continue from its end.
    */
  def prefix(n: Int, graph: Graph): Path =
    val taken = nodes.take(n + 1)
    Path(taken, Path.costOf(taken, graph))

  /** True if `other` starts with exactly the same node sequence as this path. */
  def sharesPrefixWith(other: Path, length: Int): Boolean =
    nodes.length >= length && other.nodes.length >= length &&
      nodes.iterator.take(length).sameElements(other.nodes.iterator.take(length))

  override def toString: String =
    val shown = if nodes.length <= 12 then nodes.mkString(" -> ")
    else s"${nodes.take(6).mkString(" -> ")} -> ... -> ${nodes.takeRight(3).mkString(" -> ")}"
    f"Path($shown, cost=$cost%.3f, hops=$hops)"

object Path:
  val empty: Path = Path(IndexedSeq.empty, Double.PositiveInfinity)

  /** Sums the cheapest edge between each consecutive pair.
    *
    * Parallel edges are legal (two roads can join the same pair of junctions),
    * so this picks the cheapest each time rather than assuming uniqueness.
    */
  def costOf(nodes: IndexedSeq[NodeId], graph: Graph): Weight =
    var total = 0.0
    var i = 0
    while i < nodes.length - 1 do
      val u = nodes(i)
      val v = nodes(i + 1)
      var best = Double.PositiveInfinity
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        if graph.target(e) == v && graph.weight(e) < best then best = graph.weight(e)
        e += 1
      total += best
      i += 1
    total

/** The result of a single-source search: distances to everything reachable,
  * plus the predecessor tree needed to reconstruct routes.
  *
  * `settled` records how many nodes the search actually popped and finalised.
  * That number is the whole point of comparing these algorithms -- Dijkstra and
  * A* return identical paths, and the interesting difference is that A* settles
  * a fraction as many nodes to get there.
  */
final case class ShortestPathTree(
    source: NodeId,
    dist: Array[Weight],
    parent: Array[NodeId],
    settled: Int
):

  def distanceTo(node: NodeId): Weight = dist(node)

  def isReachable(node: NodeId): Boolean = !dist(node).isInfinite

  def pathTo(node: NodeId): Option[Path] =
    if !isReachable(node) then None
    else
      val buf = mutable.ArrayBuffer.empty[NodeId]
      var cur = node
      while cur != NoNode do
        buf += cur
        cur = parent(cur)
      Some(Path(buf.reverse.toIndexedSeq, dist(node)))

  def reachableCount: Int = dist.count(d => !d.isInfinite)

  override def toString: String =
    s"ShortestPathTree(source=$source, reachable=$reachableCount, settled=$settled)"

/** A point-to-point search result.
  *
  * Carries `settled` for the same reason [[ShortestPathTree]] does: so a demo
  * can print "Dijkstra settled 41,203 nodes, A* settled 3,118" and make the
  * value of a good heuristic immediately legible.
  */
final case class SearchResult(path: Option[Path], settled: Int):
  def cost: Weight = path.map(_.cost).getOrElse(Double.PositiveInfinity)
  def found: Boolean = path.isDefined
