package mapfun

import mapfun.graph.*
import mapfun.algo.*
import mapfun.gen.Generators
import munit.FunSuite

/** The central claim of this project is that several very different algorithms
  * compute the same thing. These tests hold them against each other rather than
  * against hand-written expected answers, which catches far more.
  */
class ShortestPathSuite extends FunSuite:

  private val tolerance = 1e-9

  test("dijkstra finds the cheap detour over the expensive direct edge") {
    val g = CsrGraph.directed(4)(
      (0, 1, 1.0),
      (1, 2, 1.0),
      (2, 3, 1.0),
      (0, 3, 10.0) // direct but expensive
    )
    val r = Dijkstra.shortestPath(g, 0, 3)
    assertEquals(r.path.map(_.nodes), Some(IndexedSeq(0, 1, 2, 3)))
    assertEqualsDouble(r.cost, 3.0, tolerance)
  }

  test("unreachable targets yield no path, not an infinite one") {
    val g = CsrGraph.directed(3)((0, 1, 1.0))
    assertEquals(Dijkstra.shortestPath(g, 0, 2).path, None)
    assert(!Dijkstra.from(g, 0).isReachable(2))
  }

  test("one-way edges are respected") {
    val g = CsrGraph.directed(2)((0, 1, 1.0))
    assert(Dijkstra.shortestPath(g, 0, 1).found)
    assert(!Dijkstra.shortestPath(g, 1, 0).found)
  }

  test("parallel edges: the cheapest one wins") {
    val g = CsrGraph.directed(2)((0, 1, 9.0), (0, 1, 2.0), (0, 1, 5.0))
    assertEqualsDouble(Dijkstra.shortestPath(g, 0, 1).cost, 2.0, tolerance)
  }

  test("BFS and Dijkstra agree exactly when every edge costs 1") {
    val g = Generators.erdosRenyi(200, 0.03, seed = 11, maxWeight = 0.0)
    // Rebuild with unit weights.
    val b = GraphBuilder(g.numNodes)
    b.ensureNodes(g.numNodes)
    g.nodeIds.foreach(u => g.foreachOutEdge(u)((v, _) => b.addEdge(u, v, 1.0)))
    val unit = b.build()

    val bfs = Bfs.from(unit, 0)
    val dij = Dijkstra.from(unit, 0)
    unit.nodeIds.foreach { n =>
      assertEqualsDouble(bfs.dist(n), dij.dist(n), tolerance, s"node $n")
    }
  }

  test("0-1 BFS agrees with Dijkstra on 0/1 weights") {
    val rng = scala.util.Random(5)
    val b = GraphBuilder(300)
    b.ensureNodes(300)
    for u <- 0 until 300; _ <- 0 until 4 do
      b.addEdge(u, rng.nextInt(300), if rng.nextBoolean() then 0.0 else 1.0)
    val g = b.build()

    val zo = ZeroOneBfs.from(g, 0)
    val dij = Dijkstra.from(g, 0)
    g.nodeIds.foreach(n => assertEqualsDouble(zo.dist(n), dij.dist(n), tolerance, s"node $n"))
  }

  test("Bellman-Ford agrees with Dijkstra when all weights are non-negative") {
    val g = Generators.erdosRenyi(150, 0.04, seed = 3)
    val dij = Dijkstra.from(g, 0)
    BellmanFord.from(g, 0) match
      case BellmanFord.Outcome.Distances(bf) =>
        g.nodeIds.foreach(n => assertEqualsDouble(bf.dist(n), dij.dist(n), 1e-7, s"node $n"))
      case other => fail(s"expected distances, got $other")
  }

  test("Bellman-Ford handles negative edges that Dijkstra would get wrong") {
    // Dijkstra settles node 2 at cost 2 and never revisits it, missing the
    // cheaper 0 -> 1 -> 2 route that dips negative.
    val g = CsrGraph.directed(3)(
      (0, 1, 5.0),
      (0, 2, 2.0),
      (1, 2, -4.0)
    )
    BellmanFord.from(g, 0) match
      case BellmanFord.Outcome.Distances(t) =>
        assertEqualsDouble(t.dist(2), 1.0, tolerance)
      case other => fail(s"expected distances, got $other")

    // And confirm Dijkstra really does get it wrong -- the reason the
    // non-negativity precondition exists.
    assertEqualsDouble(Dijkstra.from(g, 0).dist(2), 2.0, tolerance)
  }

  test("Bellman-Ford reports a negative cycle") {
    val g = CsrGraph.directed(4)(
      (0, 1, 1.0),
      (1, 2, -2.0),
      (2, 3, -3.0),
      (3, 1, 1.0) // cycle 1->2->3->1 sums to -4
    )
    BellmanFord.from(g, 0) match
      case BellmanFord.Outcome.NegativeCycle(affected, witness) =>
        assert(affected.nonEmpty, "should report affected nodes")
        assert(witness.nonEmpty, "should report a witness cycle")
        assert(Set(1, 2, 3).subsetOf(affected))
      case other => fail(s"expected a negative cycle, got $other")
  }

  test("Floyd-Warshall agrees with Dijkstra from every source") {
    val g = Generators.erdosRenyi(120, 0.05, seed = 21)
    val fw = FloydWarshall(g)
    g.nodeIds.foreach { s =>
      val dij = Dijkstra.from(g, s)
      g.nodeIds.foreach { t =>
        assertEqualsDouble(fw.distance(s, t), dij.dist(t), 1e-7, s"$s -> $t")
      }
    }
  }

  test("Floyd-Warshall reconstructs paths whose costs match its matrix") {
    val g = Generators.erdosRenyi(60, 0.06, seed = 9)
    val fw = FloydWarshall(g)
    for s <- 0 until 60; t <- 0 until 60 if s != t do
      fw.path(s, t) match
        case Some(p) =>
          assertEqualsDouble(Path.costOf(p.nodes, g), fw.distance(s, t), 1e-7, s"$s -> $t")
          assertEquals(p.nodes.head, s)
          assertEquals(p.nodes.last, t)
        case None =>
          assert(fw.distance(s, t).isInfinite, s"$s -> $t: no path but finite distance")
  }

  test("A* with the zero heuristic is exactly Dijkstra") {
    val g = Generators.erdosRenyi(200, 0.03, seed = 4)
    val dij = Dijkstra.shortestPath(g, 0, 150)
    val astar = AStar.search(g, 0, 150, Heuristic.zero)
    assertEqualsDouble(astar.cost, dij.cost, tolerance)
  }

  test("A* on a grid returns the optimal cost while settling far fewer nodes") {
    val geo = Generators.grid(60, 60)
    val source = 0
    val target = geo.numNodes - 1

    val dij = Dijkstra.shortestPath(geo.graph, source, target)
    val astar = AStar.search(geo.graph, source, target, geo.heuristicTo(target))

    assertEqualsDouble(astar.cost, dij.cost, 1e-6)
    assert(
      astar.settled < dij.settled,
      s"A* settled ${astar.settled}, Dijkstra settled ${dij.settled} -- the heuristic bought nothing"
    )
  }

  test("A* matches Dijkstra across many random grid queries") {
    val geo = Generators.grid(30, 30)
    val rng = scala.util.Random(77)
    for _ <- 0 until 60 do
      val s = rng.nextInt(geo.numNodes)
      val t = rng.nextInt(geo.numNodes)
      val d = Dijkstra.shortestPath(geo.graph, s, t)
      val a = AStar.search(geo.graph, s, t, geo.heuristicTo(t))
      assertEqualsDouble(a.cost, d.cost, 1e-6, s"$s -> $t")
  }

  test("A* stays optimal on a maze, where the heuristic is nearly useless") {
    val geo = Generators.maze(25, 25, seed = 5)
    val rng = scala.util.Random(2)
    for _ <- 0 until 20 do
      val s = rng.nextInt(geo.numNodes)
      val t = rng.nextInt(geo.numNodes)
      val d = Dijkstra.shortestPath(geo.graph, s, t)
      val a = AStar.search(geo.graph, s, t, geo.heuristicTo(t))
      assertEqualsDouble(a.cost, d.cost, 1e-6, s"$s -> $t")
  }

  test("bidirectional Dijkstra matches Dijkstra on random geometric graphs") {
    val geo = Generators.randomGeometric(500, 900.0, seed = 8)
    val rev = geo.graph.reverse
    val rng = scala.util.Random(13)
    for _ <- 0 until 50 do
      val s = rng.nextInt(geo.numNodes)
      val t = rng.nextInt(geo.numNodes)
      val one = Dijkstra.shortestPath(geo.graph, s, t)
      val two = BidirectionalDijkstra.search(geo.graph, rev, s, t)
      assertEquals(two.found, one.found, s"$s -> $t reachability disagrees")
      if one.found then assertEqualsDouble(two.cost, one.cost, 1e-6, s"$s -> $t")
  }

  test("bidirectional Dijkstra returns a genuine, connected path") {
    val geo = Generators.grid(40, 40)
    val rev = geo.graph.reverse
    val result = BidirectionalDijkstra.search(geo.graph, rev, 0, geo.numNodes - 1)
    val path = result.path.get
    assertEquals(path.nodes.head, 0)
    assertEquals(path.nodes.last, geo.numNodes - 1)
    // Every consecutive pair must really be an edge.
    assertEqualsDouble(Path.costOf(path.nodes, geo.graph), path.cost, 1e-6)
  }

  test("bidirectional handles source == target and disconnected pairs") {
    val g = CsrGraph.directed(3)((0, 1, 1.0))
    val rev = g.reverse
    assertEqualsDouble(BidirectionalDijkstra.search(g, rev, 1, 1).cost, 0.0, tolerance)
    assert(!BidirectionalDijkstra.search(g, rev, 0, 2).found)
  }

  test("search constraints exclude banned nodes") {
    val g = CsrGraph.directed(4)(
      (0, 1, 1.0),
      (1, 3, 1.0),
      (0, 2, 5.0),
      (2, 3, 5.0)
    )
    val open = Dijkstra.shortestPath(g, 0, 3)
    assertEqualsDouble(open.cost, 2.0, tolerance)

    val detour = Dijkstra.shortestPath(g, 0, 3, SearchConstraints(nodeAllowed = _ != 1))
    assertEqualsDouble(detour.cost, 10.0, tolerance)
  }

  test("Yen returns k distinct loopless paths in non-decreasing cost order") {
    val g = CsrGraph.directed(6)(
      (0, 1, 1.0),
      (0, 2, 2.0),
      (1, 3, 1.0),
      (2, 3, 1.0),
      (3, 4, 1.0),
      (1, 4, 5.0),
      (4, 5, 1.0)
    )
    val paths = Yen.shortestPaths(g, 0, 5, 3)
    assert(paths.length >= 2, s"expected several routes, got ${paths.length}")

    // Non-decreasing cost.
    paths.sliding(2).foreach {
      case Seq(a, b) => assert(a.cost <= b.cost + 1e-9, s"out of order: ${a.cost} then ${b.cost}")
      case _         => ()
    }
    // Distinct.
    assertEquals(paths.map(_.nodes).distinct.length, paths.length)
    // Loopless, correctly costed, and actually connected.
    paths.foreach { p =>
      assertEquals(p.nodes.distinct.length, p.nodes.length, s"path revisits a node: $p")
      assertEqualsDouble(Path.costOf(p.nodes, g), p.cost, 1e-9)
      assertEquals(p.nodes.head, 0)
      assertEquals(p.nodes.last, 5)
    }
    // The first is the true shortest path.
    assertEqualsDouble(paths.head.cost, Dijkstra.shortestPath(g, 0, 5).cost, 1e-9)
  }

  test("Yen returns fewer than k when fewer exist rather than looping forever") {
    val g = CsrGraph.directed(3)((0, 1, 1.0), (1, 2, 1.0))
    val paths = Yen.shortestPaths(g, 0, 2, 5)
    assertEquals(paths.length, 1)
  }

  test("Yen's first path agrees with Dijkstra on a grid") {
    val geo = Generators.grid(12, 12)
    val target = geo.numNodes - 1
    val paths = Yen.shortestPaths(geo.graph, 0, target, 3)
    assertEqualsDouble(
      paths.head.cost,
      Dijkstra.shortestPath(geo.graph, 0, target).cost,
      1e-6
    )
    assertEquals(paths.map(_.nodes).distinct.length, paths.length)
  }
