package mapfun

import mapfun.graph.*
import mapfun.algo.*
import mapfun.gen.Generators
import munit.FunSuite

class TraversalSuite extends FunSuite:

  test("DFS preorder starts at the source and visits only reachable nodes") {
    val g = CsrGraph.directed(5)((0, 1, 1.0), (1, 2, 1.0), (3, 4, 1.0))
    val order = Traversal.dfsPreorder(g, 0)
    assertEquals(order.head, 0)
    assertEquals(order.toSet, Set(0, 1, 2))
  }

  test("DFS postorder finishes descendants before their ancestors") {
    val g = CsrGraph.directed(4)((0, 1, 1.0), (1, 2, 1.0), (2, 3, 1.0))
    assertEquals(Traversal.dfsPostorder(g, 0), Vector(3, 2, 1, 0))
  }

  test("iterative DFS survives a path far deeper than the JVM stack") {
    // A recursive DFS overflows here. This is the entire reason the
    // implementation carries an explicit stack.
    val n = 300000
    val b = GraphBuilder(n)
    (0 until n - 1).foreach(i => b.addEdge(i, i + 1, 1.0))
    val g = b.build()
    assertEquals(Traversal.dfsPreorder(g, 0).length, n)
  }

  test("topological sort orders every edge forwards") {
    val g = Generators.randomDag(200, 3, seed = 6)
    val order = Traversal.topologicalSort(g).get
    assertEquals(order.length, g.numNodes)

    val rank = new Array[Int](g.numNodes)
    order.zipWithIndex.foreach((n, i) => rank(n) = i)
    g.nodeIds.foreach { u =>
      g.foreachOutEdge(u)((v, _) => assert(rank(u) < rank(v), s"edge $u -> $v points backwards"))
    }
  }

  test("topological sort refuses a cyclic graph") {
    val g = CsrGraph.directed(3)((0, 1, 1.0), (1, 2, 1.0), (2, 0, 1.0))
    assertEquals(Traversal.topologicalSort(g), None)
    assert(Traversal.hasCycle(g))
  }

  test("a DAG has no cycle") {
    assert(!Traversal.hasCycle(Generators.randomDag(100, 2, seed = 1)))
  }

  test("even cycles are bipartite, odd ones are not") {
    val even = CsrGraph.undirected(4)((0, 1, 1.0), (1, 2, 1.0), (2, 3, 1.0), (3, 0, 1.0))
    assert(Traversal.isBipartite(even))

    val odd = CsrGraph.undirected(3)((0, 1, 1.0), (1, 2, 1.0), (2, 0, 1.0))
    assert(!Traversal.isBipartite(odd))
  }

  test("a grid is bipartite and its colouring is a genuine two-colouring") {
    val geo = Generators.grid(20, 20)
    val colors = Traversal.twoColoring(geo.graph).get
    geo.graph.nodeIds.foreach { u =>
      geo.graph.foreachOutEdge(u)((v, _) => assertNotEquals(colors(u), colors(v)))
    }
  }

  test("connected components separate disjoint pieces") {
    val g = CsrGraph.undirected(6)((0, 1, 1.0), (1, 2, 1.0), (3, 4, 1.0))
    val comps = Traversal.connectedComponents(g)
    assertEquals(comps(0), comps(2))
    assertNotEquals(comps(0), comps(3))
    assertEquals(comps.distinct.length, 3) // {0,1,2} {3,4} {5}
  }

  test("Tarjan finds strongly connected components") {
    // 0 <-> 1 form one SCC; 2 -> 3 -> 4 -> 2 form another; the link is one-way.
    val g = CsrGraph.directed(5)(
      (0, 1, 1.0),
      (1, 0, 1.0),
      (1, 2, 1.0),
      (2, 3, 1.0),
      (3, 4, 1.0),
      (4, 2, 1.0)
    )
    val r = Tarjan.sccs(g)
    assertEquals(r.count, 2)
    assertEquals(r.componentOf(0), r.componentOf(1))
    assertEquals(r.componentOf(2), r.componentOf(3))
    assertEquals(r.componentOf(3), r.componentOf(4))
    assertNotEquals(r.componentOf(0), r.componentOf(2))
  }

  test("a DAG's SCCs are all singletons") {
    val g = Generators.randomDag(200, 3, seed = 12)
    val r = Tarjan.sccs(g)
    assertEquals(r.count, g.numNodes)
  }

  test("an undirected connected graph is one SCC") {
    val geo = Generators.grid(15, 15)
    assertEquals(Tarjan.sccs(geo.graph).count, 1)
  }

  test("Tarjan agrees with reachability: same SCC iff mutually reachable") {
    val g = Generators.erdosRenyi(120, 0.02, seed = 31)
    val r = Tarjan.sccs(g)
    val reach = Array.tabulate(g.numNodes)(s => Dijkstra.from(g, s))
    val rng = scala.util.Random(3)
    for _ <- 0 until 400 do
      val u = rng.nextInt(g.numNodes)
      val v = rng.nextInt(g.numNodes)
      val mutual = reach(u).isReachable(v) && reach(v).isReachable(u)
      assertEquals(r.componentOf(u) == r.componentOf(v), mutual, s"$u vs $v")
  }

  test("Tarjan survives a deep chain without stack overflow") {
    val n = 200000
    val b = GraphBuilder(n)
    (0 until n - 1).foreach(i => b.addEdge(i, i + 1, 1.0))
    val g = b.build()
    assertEquals(Tarjan.sccs(g).count, n)
  }

  test("largest SCC extraction yields a fully mutually reachable subgraph") {
    val g = Generators.erdosRenyi(300, 0.01, seed = 44)
    val (sub, _) = Tarjan.largestSccSubgraph(g)
    if sub.numNodes > 1 then
      assertEquals(Tarjan.sccs(sub).count, 1)
      // Every node reaches every other -- the property that makes routing safe.
      val from0 = Dijkstra.from(sub, 0)
      assertEquals(from0.reachableCount, sub.numNodes)
  }

  test("Kruskal and Prim find equal-weight spanning forests") {
    val geo = Generators.randomGeometric(300, 1200.0, seed = 17)
    val k = Kruskal.minimumSpanningForest(geo.graph)
    val p = Prim.minimumSpanningForest(geo.graph)
    assertEqualsDouble(k.map(_.weight).sum, p.map(_.weight).sum, 1e-6)
    assertEquals(k.length, p.length)
  }

  test("an MST spans every node and contains no cycle") {
    val geo = Generators.grid(20, 20)
    val mst = Kruskal.minimumSpanningForest(geo.graph)
    assertEquals(mst.length, geo.numNodes - 1) // connected graph: a tree

    val uf = UnionFind(geo.numNodes)
    mst.foreach(e => assert(uf.union(e.from, e.to), s"cycle via ${e.from}-${e.to}"))
    assertEquals(uf.components, 1)
  }

  test("MST of a disconnected graph is a forest, one tree per component") {
    val g = CsrGraph.undirected(6)((0, 1, 1.0), (1, 2, 2.0), (3, 4, 1.0))
    val mst = Kruskal.minimumSpanningForest(g)
    // 3 components ({0,1,2}, {3,4}, {5}) so 6 - 3 = 3 edges.
    assertEquals(mst.length, 3)
  }

  test("MST beats or ties any spanning tree we can build greedily by insertion order") {
    val geo = Generators.randomGeometric(200, 1500.0, seed = 23)
    val mstWeight = Kruskal.minimumSpanningForest(geo.graph).map(_.weight).sum

    // Same algorithm without the sort: valid spanning forest, not minimal.
    val uf = UnionFind(geo.numNodes)
    var naive = 0.0
    geo.graph.nodeIds.foreach { u =>
      geo.graph.foreachOutEdge(u) { (v, w) =>
        if u < v && uf.union(u, v) then naive += w
      }
    }
    assert(mstWeight <= naive + 1e-9, s"MST $mstWeight should not exceed unsorted $naive")
  }
