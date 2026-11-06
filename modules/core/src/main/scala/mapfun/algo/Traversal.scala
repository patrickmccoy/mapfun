package mapfun.algo

import mapfun.graph.*

/** Depth-first search and the things you get almost for free from its visit
  * order: topological sorts, cycle detection, two-colouring.
  *
  * Every DFS here is iterative. A recursive one is shorter and reads better,
  * but the recursion depth is the length of the longest simple path, and a road
  * network has paths hundreds of thousands of nodes long -- it will overflow
  * the stack on real data, not just in theory. The explicit stack stores a
  * frame per node as two parallel `Int` arrays: which node, and how far through
  * its edge slots the traversal had got. That second number is the entire
  * reason [[Graph]] exposes edge slots rather than neighbour collections.
  */
object Traversal:

  /** Order in which nodes are first reached. */
  def dfsPreorder(graph: Graph, source: NodeId): Vector[NodeId] =
    val order = Vector.newBuilder[NodeId]
    dfs(graph, source)(onDiscover = n => order += n, onFinish = _ => ())
    order.result()

  /** Order in which nodes are finished -- all descendants explored.
    *
    * Reverse postorder on a DAG is a topological order, which is the
    * observation [[topologicalSort]] is built on.
    */
  def dfsPostorder(graph: Graph, source: NodeId): Vector[NodeId] =
    val order = Vector.newBuilder[NodeId]
    dfs(graph, source)(onDiscover = _ => (), onFinish = n => order += n)
    order.result()

  /** Iterative DFS from a single source, with hooks on discovery and finish. */
  def dfs(graph: Graph, source: NodeId)(
      onDiscover: NodeId => Unit,
      onFinish: NodeId => Unit
  ): Array[Boolean] =
    val visited = new Array[Boolean](graph.numNodes)
    dfsFrom(graph, source, visited, onDiscover, onFinish)
    visited

  private def dfsFrom(
      graph: Graph,
      source: NodeId,
      visited: Array[Boolean],
      onDiscover: NodeId => Unit,
      onFinish: NodeId => Unit
  ): Unit =
    if visited(source) then return
    val n = graph.numNodes
    val stackNode = new Array[Int](n + 1)
    val stackEdge = new Array[Int](n + 1)
    var sp = 0

    visited(source) = true
    onDiscover(source)
    stackNode(0) = source
    stackEdge(0) = graph.edgeBegin(source)

    while sp >= 0 do
      val u = stackNode(sp)
      if stackEdge(sp) < graph.edgeEnd(u) then
        val e = stackEdge(sp)
        stackEdge(sp) = e + 1 // resume here when we come back to this frame
        val v = graph.target(e)
        if !visited(v) then
          visited(v) = true
          onDiscover(v)
          sp += 1
          stackNode(sp) = v
          stackEdge(sp) = graph.edgeBegin(v)
      else
        onFinish(u)
        sp -= 1

  /** Weakly connected components, treating every edge as undirected.
    *
    * "Weakly" matters: on a road network with one-way streets, two junctions
    * can be weakly connected and still have no legal route between them. For
    * routing you want [[Tarjan]]'s strongly connected components instead.
    */
  def connectedComponents(graph: Graph): Array[Int] =
    val uf = UnionFind(graph.numNodes)
    var u = 0
    while u < graph.numNodes do
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        uf.union(u, graph.target(e))
        e += 1
      u += 1
    uf.componentLabels

  /** A topological order, or `None` if the graph has a directed cycle.
    *
    * Kahn's algorithm: repeatedly emit a node with no remaining incoming edges.
    * If fewer than `numNodes` come out, whatever is left is a cycle. Chosen over
    * the DFS formulation because the leftover set is directly useful as a
    * diagnostic.
    */
  def topologicalSort(graph: Graph): Option[Vector[NodeId]] =
    val n = graph.numNodes
    val inDegree = new Array[Int](n)
    var u = 0
    while u < n do
      var e = graph.edgeBegin(u)
      val end = graph.edgeEnd(u)
      while e < end do
        inDegree(graph.target(e)) += 1
        e += 1
      u += 1

    val ready = new Array[Int](n)
    var head = 0
    var tail = 0
    u = 0
    while u < n do
      if inDegree(u) == 0 then
        ready(tail) = u
        tail += 1
      u += 1

    val order = Vector.newBuilder[NodeId]
    var emitted = 0
    while head < tail do
      val v = ready(head)
      head += 1
      order += v
      emitted += 1
      var e = graph.edgeBegin(v)
      val end = graph.edgeEnd(v)
      while e < end do
        val w = graph.target(e)
        inDegree(w) -= 1
        if inDegree(w) == 0 then
          ready(tail) = w
          tail += 1
        e += 1

    if emitted == n then Some(order.result()) else None

  def hasCycle(graph: Graph): Boolean = topologicalSort(graph).isEmpty

  /** Two-colours the graph, treating edges as undirected.
    *
    * Returns the colouring, or `None` if an odd cycle makes it impossible.
    */
  def twoColoring(graph: Graph): Option[Array[Int]] =
    val n = graph.numNodes
    val color = Array.fill(n)(-1)
    val queue = new Array[Int](n)
    var ok = true

    var start = 0
    while start < n && ok do
      if color(start) == -1 then
        color(start) = 0
        var head = 0
        var tail = 0
        queue(tail) = start
        tail += 1
        while head < tail && ok do
          val u = queue(head)
          head += 1
          var e = graph.edgeBegin(u)
          val end = graph.edgeEnd(u)
          while e < end do
            val v = graph.target(e)
            if color(v) == -1 then
              color(v) = 1 - color(u)
              queue(tail) = v
              tail += 1
            else if color(v) == color(u) then ok = false
            e += 1
      start += 1

    if ok then Some(color) else None

  def isBipartite(graph: Graph): Boolean = twoColoring(graph).isDefined

/** Strongly connected components via Tarjan's algorithm, in one pass.
  *
  * Two nodes are in the same SCC when each can reach the other. On a road
  * network this is the single most important preprocessing step: an extract
  * clipped to a bounding box is full of stubs, one-way slip roads whose
  * continuation was cut off, and service roads reachable only in one direction.
  * Route into one of those and the search simply fails. Keeping only the
  * largest SCC removes the whole class of problem.
  *
  * Written iteratively for the same stack-depth reason as [[Traversal.dfs]],
  * which makes it longer than the textbook recursion but survives real input.
  */
object Tarjan:

  /** `componentOf(node)` is the component id; ids are dense in `[0, count)`.
    *
    * Tarjan emits components in reverse topological order of the condensation
    * graph, so component ids are meaningful, not arbitrary.
    */
  final case class Result(componentOf: Array[Int], count: Int):

    def sizes: Array[Int] =
      val s = new Array[Int](count)
      var i = 0
      while i < componentOf.length do
        s(componentOf(i)) += 1
        i += 1
      s

    /** Id of the component with the most nodes. */
    def largestComponent: Int =
      val s = sizes
      var best = 0
      var i = 1
      while i < count do
        if s(i) > s(best) then best = i
        i += 1
      best

    def members(component: Int): Array[NodeId] =
      componentOf.zipWithIndex.collect { case (c, n) if c == component => n }

  def sccs(graph: Graph): Result =
    val n = graph.numNodes
    val index = Array.fill(n)(-1) // discovery order, -1 = unvisited
    val lowlink = new Array[Int](n) // lowest index reachable from this subtree
    val onStack = new Array[Boolean](n)
    val component = Array.fill(n)(-1)

    val sccStack = new Array[Int](n)
    var sccTop = 0

    val frameNode = new Array[Int](n + 1)
    val frameEdge = new Array[Int](n + 1)

    var nextIndex = 0
    var componentCount = 0

    var root = 0
    while root < n do
      if index(root) == -1 then
        var sp = 0
        index(root) = nextIndex
        lowlink(root) = nextIndex
        nextIndex += 1
        sccStack(sccTop) = root
        sccTop += 1
        onStack(root) = true
        frameNode(0) = root
        frameEdge(0) = graph.edgeBegin(root)

        while sp >= 0 do
          val u = frameNode(sp)
          if frameEdge(sp) < graph.edgeEnd(u) then
            val e = frameEdge(sp)
            frameEdge(sp) = e + 1
            val v = graph.target(e)
            if index(v) == -1 then
              // Tree edge: descend.
              index(v) = nextIndex
              lowlink(v) = nextIndex
              nextIndex += 1
              sccStack(sccTop) = v
              sccTop += 1
              onStack(v) = true
              sp += 1
              frameNode(sp) = v
              frameEdge(sp) = graph.edgeBegin(v)
            else if onStack(v) then
              // Back/cross edge into the current SCC candidate.
              if index(v) < lowlink(u) then lowlink(u) = index(v)
          else
            // u is finished. If nothing below it reached higher than u itself,
            // u roots an SCC and everything above it on the stack belongs to it.
            if lowlink(u) == index(u) then
              var w = NoNode
              while w != u do
                sccTop -= 1
                w = sccStack(sccTop)
                onStack(w) = false
                component(w) = componentCount
              componentCount += 1
            sp -= 1
            if sp >= 0 then
              val parent = frameNode(sp)
              if lowlink(u) < lowlink(parent) then lowlink(parent) = lowlink(u)
      root += 1

    Result(component, componentCount)

  /** Keeps only the largest strongly connected component.
    *
    * Returns the subgraph and old-id -> new-id mapping (`NoNode` for dropped
    * nodes), matching [[mapfun.graph.CsrGraph.subgraph]].
    */
  def largestSccSubgraph(graph: CsrGraph): (CsrGraph, Array[NodeId]) =
    val result = sccs(graph)
    if result.count == 0 then (graph, Array.tabulate(graph.numNodes)(identity))
    else
      val keep = result.largestComponent
      graph.subgraph(n => result.componentOf(n) == keep)
