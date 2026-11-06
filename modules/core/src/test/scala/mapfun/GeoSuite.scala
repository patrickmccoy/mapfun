package mapfun

import mapfun.geo.*
import mapfun.graph.*
import mapfun.algo.*
import mapfun.gen.Generators
import munit.FunSuite

class GeoSuite extends FunSuite:

  private val providence = LatLon(41.8240, -71.4128)
  private val boston = LatLon(42.3601, -71.0589)

  test("haversine matches the known Providence-Boston distance") {
    // ~66 km great-circle.
    val d = Haversine.distance(providence, boston)
    assert(d > 64000 && d < 68000, s"got $d metres")
  }

  test("haversine is zero for identical points and symmetric otherwise") {
    assertEqualsDouble(Haversine.distance(providence, providence), 0.0, 1e-9)
    assertEqualsDouble(
      Haversine.distance(providence, boston),
      Haversine.distance(boston, providence),
      1e-6
    )
  }

  test("haversine satisfies the triangle inequality") {
    val mid = LatLon(42.0, -71.2)
    val direct = Haversine.distance(providence, boston)
    val viaMid = Haversine.distance(providence, mid) + Haversine.distance(mid, boston)
    assert(direct <= viaMid + 1e-6, s"direct $direct exceeded detour $viaMid")
  }

  test("the flat approximation is close to haversine over short distances") {
    val a = LatLon(41.8240, -71.4128)
    val b = LatLon(41.8300, -71.4000)
    val exact = Haversine.distance(a, b)
    val approx = Haversine.approximate(a, b)
    assert(math.abs(exact - approx) / exact < 0.001, s"exact $exact vs approx $approx")
  }

  test("Mercator round-trips") {
    val p = LatLon(41.8240, -71.4128)
    val (x, y) = Mercator.project(p)
    val back = Mercator.unproject(x, y)
    assertEqualsDouble(back.lat, p.lat, 1e-9)
    assertEqualsDouble(back.lon, p.lon, 1e-9)
  }

  test("Mercator maps the origin to the centre of the unit square") {
    val (x, y) = Mercator.project(LatLon(0.0, 0.0))
    assertEqualsDouble(x, 0.5, 1e-12)
    assertEqualsDouble(y, 0.5, 1e-12)
  }

  test("grid index finds the same nearest node as a linear scan") {
    val geo = Generators.randomGeometric(2000, 300.0, seed = 19)
    val rng = scala.util.Random(101)
    val (sw, ne) = geo.boundingBox

    for _ <- 0 until 200 do
      val query = LatLon(
        sw.lat + rng.nextDouble() * (ne.lat - sw.lat),
        sw.lon + rng.nextDouble() * (ne.lon - sw.lon)
      )
      val viaIndex = geo.nearestNode(query)
      val viaScan = geo.positions.indices.minBy(n => Haversine.distance(query, geo.position(n)))
      assertEqualsDouble(
        Haversine.distance(query, geo.position(viaIndex)),
        Haversine.distance(query, geo.position(viaScan)),
        1e-6,
        s"query $query"
      )
  }

  test("grid index handles queries outside the bounding box") {
    val geo = Generators.grid(10, 10)
    val far = LatLon(0.0, 0.0)
    assertNotEquals(geo.nearestNode(far), NoNode)
  }

  test("the distance heuristic never overestimates on a grid (admissibility)") {
    val geo = Generators.grid(25, 25)
    val target = geo.numNodes - 1
    val h = geo.heuristicTo(target)
    val truth = Dijkstra.from(geo.graph, target, constraints = SearchConstraints.unrestricted)
    // Grid edges are undirected, so distance to the target equals distance from it.
    geo.graph.nodeIds.foreach { n =>
      if truth.isReachable(n) then
        assert(
          h.estimate(n) <= truth.dist(n) + 1e-6,
          s"node $n: heuristic ${h.estimate(n)} exceeded true ${truth.dist(n)}"
        )
    }
  }

  test("the travel-time heuristic is admissible when divided by the top speed") {
    val base = Generators.grid(20, 20)
    val maxSpeed = 30.0 // m/s
    // Re-weight every edge as time at a speed no greater than maxSpeed.
    val b = GraphBuilder(base.numNodes)
    b.ensureNodes(base.numNodes)
    base.graph.nodeIds.foreach { u =>
      base.graph.foreachOutEdge(u)((v, metres) => b.addEdge(u, v, metres / 15.0))
    }
    val timed = GeoGraph(b.build(), base.positions, "timed", WeightMetric.Seconds(maxSpeed))

    val target = timed.numNodes - 1
    val h = timed.heuristicTo(target)
    val truth = Dijkstra.from(timed.graph, target)
    timed.graph.nodeIds.foreach { n =>
      if truth.isReachable(n) then
        assert(h.estimate(n) <= truth.dist(n) + 1e-6, s"node $n overestimated")
    }
  }

  test("GeoGraph.filter keeps positions aligned with renumbered nodes") {
    val geo = Generators.grid(10, 10)
    val kept = geo.filter(n => n % 2 == 0)
    assertEquals(kept.numNodes, 50)
    assertEquals(kept.positions.length, kept.numNodes)
    // Node 0 survives as node 0 and keeps its coordinates.
    assertEquals(kept.position(0), geo.position(0))
  }

  test("generators produce graphs with the promised shape") {
    val g = Generators.grid(5, 4)
    assertEquals(g.numNodes, 20)
    // Corners have degree 2, edges 3, interior 4.
    assertEquals(g.graph.outDegree(0), 2)

    val m = Generators.maze(10, 10, seed = 3)
    // A perfect maze on n cells is a tree: n - 1 undirected edges, stored twice.
    assertEquals(m.graph.numEdges, 2 * (100 - 1))
    assertEquals(Traversal.connectedComponents(m.graph).distinct.length, 1)
  }

  test("GeoJSON output is well-formed and uses lon,lat order") {
    val geo = Generators.grid(5, 5)
    val path = Dijkstra.shortestPath(geo.graph, 0, 24).path.get
    val json = mapfun.io.GeoJson.collection(Seq(mapfun.io.GeoJson.path(geo, path)))

    assert(json.startsWith("""{"type":"FeatureCollection"""))
    assert(json.contains("LineString"))
    assertEquals(json.count(_ == '{'), json.count(_ == '}'))
    assertEquals(json.count(_ == '['), json.count(_ == ']'))
    // Longitudes here are around -71, latitudes around +41: lon must come first.
    assert(json.contains("[-71."), "coordinates should lead with longitude")
  }
