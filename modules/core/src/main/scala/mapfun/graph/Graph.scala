package mapfun.graph

/** A node is a dense integer id in `[0, numNodes)`.
  *
  * Real map data does not look like this: OpenStreetMap ids are 64-bit and
  * scattered across the whole `Long` range. But every algorithm here wants to
  * use a node id as an array index, so importers squash external ids down into
  * a dense range exactly once (see [[NodeCatalog]]) and the algorithms only
  * ever deal in `Int`s. That single decision is worth more to routing
  * performance than any amount of micro-optimisation inside the search loops.
  */
type NodeId = Int

/** An edge cost: metres, seconds, or anything else additive.
  *
  * Non-negative for everything except [[mapfun.algo.BellmanFord]], which is
  * here precisely because it is the one algorithm that tolerates negatives.
  */
type Weight = Double

/** Sentinel for "no such node" -- an absent parent, an unreachable target. */
val NoNode: NodeId = -1

/** A directed, weighted graph, addressed the way a real routing engine
  * addresses one.
  *
  * The interface exposes *edge slots* rather than returning a collection of
  * neighbours. Every outgoing edge of a node occupies a contiguous run of slot
  * indices `[edgeBegin(n), edgeEnd(n))`, and `target`/`weight` read out of that
  * slot. Two things fall out of this that matter:
  *
  *   - Iterating a node's neighbours allocates nothing. On a graph with a few
  *     million edges, allocating a `Seq` per node visit costs more than the
  *     search itself.
  *   - A partially-finished iteration is just an `Int`. That is what makes the
  *     iterative (non-recursive) DFS in [[mapfun.algo.Traversal]] and
  *     [[mapfun.algo.Tarjan]] possible -- and you need those, because a
  *     recursive DFS blows the stack on a real road network.
  *
  * The shape is deliberately that of [[CsrGraph]], the compressed-sparse-row
  * layout below. An adjacency-`Map` graph could not implement this cheaply,
  * which is the point: this interface encodes a choice about storage.
  */
trait Graph:

  def numNodes: Int

  def numEdges: Int

  /** Index of the first outgoing edge slot of `node`, inclusive. */
  def edgeBegin(node: NodeId): Int

  /** Index one past the last outgoing edge slot of `node`. */
  def edgeEnd(node: NodeId): Int

  /** The node an edge slot points at. */
  def target(edge: Int): NodeId

  /** The cost of traversing an edge slot. */
  def weight(edge: Int): Weight

  final def outDegree(node: NodeId): Int = edgeEnd(node) - edgeBegin(node)

  final def nodeIds: Range = 0 until numNodes

  /** Convenience wrapper for when clarity beats avoiding a closure. The search
    * algorithms use the raw slot loop instead.
    */
  final def foreachOutEdge(node: NodeId)(f: (NodeId, Weight) => Unit): Unit =
    var e = edgeBegin(node)
    val end = edgeEnd(node)
    while e < end do
      f(target(e), weight(e))
      e += 1

  /** Materialises a node's neighbours. Fine for tests and printing, wasteful in
    * a hot loop.
    */
  final def neighbors(node: NodeId): Vector[(NodeId, Weight)] =
    val b = Vector.newBuilder[(NodeId, Weight)]
    foreachOutEdge(node)((t, w) => b += ((t, w)))
    b.result()

  final def isEmpty: Boolean = numNodes == 0

  override def toString: String =
    s"Graph(${numNodes} nodes, ${numEdges} edges)"

/** Compressed sparse row storage: three flat arrays, no per-node objects.
  *
  * `offsets` has length `numNodes + 1`; node `n` owns edge slots
  * `offsets(n) until offsets(n + 1)`. `targets` and `weights` are parallel
  * arrays over those slots.
  *
  * The whole graph is three allocations regardless of size. A road network of
  * two million nodes costs roughly `4 * (2M + 1)` bytes of offsets plus
  * `12` bytes per edge -- tens of megabytes, and all of it contiguous, so
  * traversing a node's neighbours is a linear scan the prefetcher can see
  * coming. The equivalent `Map[NodeId, List[(NodeId, Weight)]]` would be an
  * order of magnitude larger and scattered across the heap.
  */
final class CsrGraph private[graph] (
    private val offsets: Array[Int],
    private val targets: Array[Int],
    private val weights: Array[Double]
) extends Graph:

  val numNodes: Int = offsets.length - 1
  val numEdges: Int = targets.length

  def edgeBegin(node: NodeId): Int = offsets(node)
  def edgeEnd(node: NodeId): Int   = offsets(node + 1)
  def target(edge: Int): NodeId    = targets(edge)
  def weight(edge: Int): Weight    = weights(edge)

  /** Every edge reversed.
    *
    * Needed by bidirectional search (which grows a search backwards from the
    * target) and by Kosaraju-style component algorithms. Costs a second copy
    * of the edge arrays, which is the usual price of making backward
    * traversal as cheap as forward traversal.
    */
  def reverse: CsrGraph =
    val b = GraphBuilder(numEdges)
    b.ensureNodes(numNodes)
    var u = 0
    while u < numNodes do
      var e = offsets(u)
      val end = offsets(u + 1)
      while e < end do
        b.addEdge(targets(e), u, weights(e))
        e += 1
      u += 1
    b.build()

  /** Keeps only the nodes for which `keep` holds, renumbering survivors into a
    * fresh dense range.
    *
    * Returns the subgraph and a map from old node id to new node id
    * (`NoNode` for dropped nodes). This is how you extract the largest
    * strongly connected component of a road network, which you almost always
    * want to do before routing on it -- see [[mapfun.algo.Tarjan]].
    */
  def subgraph(keep: NodeId => Boolean): (CsrGraph, Array[NodeId]) =
    val remap = Array.fill(numNodes)(NoNode)
    var next = 0
    var u = 0
    while u < numNodes do
      if keep(u) then
        remap(u) = next
        next += 1
      u += 1

    val b = GraphBuilder(numEdges)
    b.ensureNodes(next)
    u = 0
    while u < numNodes do
      val nu = remap(u)
      if nu != NoNode then
        var e = offsets(u)
        val end = offsets(u + 1)
        while e < end do
          val nv = remap(targets(e))
          if nv != NoNode then b.addEdge(nu, nv, weights(e))
          e += 1
      u += 1
    (b.build(), remap)

object CsrGraph:

  /** Builds a graph from an explicit edge list. Handy in tests. */
  def directed(numNodes: Int)(edges: (NodeId, NodeId, Weight)*): CsrGraph =
    val b = GraphBuilder(edges.length)
    b.ensureNodes(numNodes)
    edges.foreach((u, v, w) => b.addEdge(u, v, w))
    b.build()

  /** As [[directed]], but every edge is added in both directions. */
  def undirected(numNodes: Int)(edges: (NodeId, NodeId, Weight)*): CsrGraph =
    val b = GraphBuilder(2 * edges.length)
    b.ensureNodes(numNodes)
    edges.foreach((u, v, w) => b.addUndirectedEdge(u, v, w))
    b.build()

/** Accumulates edges, then packs them into a [[CsrGraph]].
  *
  * Edges arrive in whatever order the caller produces them; `build` sorts them
  * by source with a counting sort, which is O(nodes + edges) and the reason
  * importing a large extract is not dominated by graph construction.
  *
  * `edgeCapacityHint` only pre-sizes the internal arrays to avoid regrowth; it
  * says nothing about how many nodes the graph has. The node count comes from
  * the largest id mentioned by an edge, or from [[ensureNodes]] when isolated
  * nodes need to survive.
  */
final class GraphBuilder(edgeCapacityHint: Int = 0):

  private var srcs    = new Array[Int](math.max(16, edgeCapacityHint))
  private var dsts    = new Array[Int](math.max(16, edgeCapacityHint))
  private var costs   = new Array[Double](math.max(16, edgeCapacityHint))
  private var count   = 0
  private var maxNode = -1

  def edgeCount: Int = count

  def addEdge(from: NodeId, to: NodeId, weight: Weight): this.type =
    require(from >= 0 && to >= 0, s"node ids must be non-negative, got ($from, $to)")
    if count == srcs.length then grow()
    srcs(count) = from
    dsts(count) = to
    costs(count) = weight
    count += 1
    if from > maxNode then maxNode = from
    if to > maxNode then maxNode = to
    this

  def addUndirectedEdge(a: NodeId, b: NodeId, weight: Weight): this.type =
    addEdge(a, b, weight)
    addEdge(b, a, weight)

  /** Reserves node ids up to `n - 1` even if no edge mentions them, so isolated
    * nodes survive into the built graph.
    */
  def ensureNodes(n: Int): this.type =
    if n - 1 > maxNode then maxNode = n - 1
    this

  private def grow(): Unit =
    val cap = srcs.length * 2
    srcs = java.util.Arrays.copyOf(srcs, cap)
    dsts = java.util.Arrays.copyOf(dsts, cap)
    costs = java.util.Arrays.copyOf(costs, cap)

  def build(): CsrGraph =
    val nodes = maxNode + 1
    val offsets = new Array[Int](nodes + 1)

    // Pass 1: histogram of out-degrees, written one slot to the right so that
    // the prefix sum below lands directly on each node's starting offset.
    var i = 0
    while i < count do
      offsets(srcs(i) + 1) += 1
      i += 1

    // Pass 2: prefix sum turns counts into start offsets.
    i = 0
    while i < nodes do
      offsets(i + 1) += offsets(i)
      i += 1

    // Pass 3: scatter each edge into its node's run, advancing a per-node
    // cursor as slots are filled.
    val cursor = offsets.clone()
    val targets = new Array[Int](count)
    val weights = new Array[Double](count)
    i = 0
    while i < count do
      val s = srcs(i)
      val p = cursor(s)
      targets(p) = dsts(i)
      weights(p) = costs(i)
      cursor(s) = p + 1
      i += 1

    new CsrGraph(offsets, targets, weights)

object GraphBuilder:
  def apply(edgeCapacityHint: Int = 0): GraphBuilder = new GraphBuilder(edgeCapacityHint)
