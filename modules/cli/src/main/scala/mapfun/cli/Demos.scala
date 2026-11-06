package mapfun.cli

import mapfun.graph.*
import mapfun.algo.*
import mapfun.geo.*
import mapfun.gen.Generators
import mapfun.io.GeoJson
import mapfun.osm.{OsmImporter, TravelProfile, CostModel}

/** Timing helper. Not a benchmark harness -- the JVM needs warming up before
  * any of these numbers mean much, which `bench` does explicitly.
  */
object Timed:
  def apply[A](label: String)(body: => A): (A, Double) =
    val start = System.nanoTime()
    val result = body
    val millis = (System.nanoTime() - start) / 1e6
    (result, millis)

  def print[A](label: String)(body: => A): A =
    val (result, millis) = apply(label)(body)
    println(f"  $label%-34s ${millis}%8.2f ms")
    result

object Demos:

  private def rule(title: String): Unit =
    println()
    println(title)
    println("-" * title.length)

  /** Walks through the algorithms on a synthetic grid, in a deliberate order:
    * traversal, then unweighted paths, then weighted, then the speedups.
    */
  def tour(): Unit =
    val geo = Generators.grid(100, 100)
    val g = geo.graph
    val source = 0
    val target = geo.numNodes - 1

    println(s"Grid 100x100: ${g.numNodes} nodes, ${g.numEdges} directed edges")
    println(f"Straight-line source to target: ${Haversine.distance(geo.position(source), geo.position(target))}%.0f m")

    rule("Traversal")
    val components = Traversal.connectedComponents(g).distinct.length
    println(s"  connected components            $components")
    println(s"  bipartite                       ${Traversal.isBipartite(g)}")
    println(s"  strongly connected components   ${Tarjan.sccs(g).count}")
    println(s"  has a directed cycle            ${Traversal.hasCycle(g)}")

    rule("Shortest path, same answer four ways")
    val dij = Timed.print("Dijkstra")(Dijkstra.shortestPath(g, source, target))
    val astar = Timed.print("A* (straight-line)")(AStar.search(g, source, target, geo.heuristicTo(target)))
    val astarL1 = Timed.print("A* (taxicab)")(AStar.search(g, source, target, geo.manhattanHeuristicTo(target)))
    val rev = g.reverse
    val bidi = Timed.print("bidirectional Dijkstra")(BidirectionalDijkstra.search(g, rev, source, target))

    println()
    println(f"  cost     ${dij.cost}%10.1f ${astar.cost}%10.1f ${astarL1.cost}%10.1f ${bidi.cost}%10.1f   (all identical)")
    println(f"  settled  ${dij.settled}%10d ${astar.settled}%10d ${astarL1.settled}%10d ${bidi.settled}%10d")

    println()
    println("  Why does straight-line A* barely beat Dijkstra here, while taxicab A*")
    println("  crushes it? On a 4-connected grid you can only move along the axes, so the")
    println("  true remaining distance is the taxicab (L1) distance. Straight-line (L2)")
    println("  distance is up to sqrt(2) smaller -- still a valid lower bound, so A* stays")
    println("  correct, but so weak that it hardly prunes anything.")
    println()
    println("  Admissibility buys you the right answer. Tightness is what buys you speed.")

    rule("Weighted A*: trading optimality for speed")
    Seq(1.0, 1.2, 1.5, 2.0, 3.0).foreach { factor =>
      val r = AStar.search(g, source, target, Heuristic.weighted(geo.heuristicTo(target), factor))
      val excess = 100.0 * (r.cost / dij.cost - 1)
      println(f"  factor $factor%.1f  settled ${r.settled}%7d  cost ${r.cost}%10.1f  (${excess}%+.2f%% vs optimal)")
    }

    rule("A road-shaped graph, where straight-line distance is informative")
    // A random geometric graph has edges that actually run towards their
    // targets, so the straight-line estimate is close to the truth -- much more
    // like a real road network than a perfect grid is.
    val geoRandom = Generators.randomGeometric(20000, 260.0, seed = 5)
    val rTarget = geoRandom.nearestNode(geoRandom.boundingBox._2)
    val rSource = geoRandom.nearestNode(geoRandom.boundingBox._1)
    val rDij = Dijkstra.shortestPath(geoRandom.graph, rSource, rTarget)
    val rAstar = AStar.search(geoRandom.graph, rSource, rTarget, geoRandom.heuristicTo(rTarget))
    if rDij.found then
      println(f"  Dijkstra settled ${rDij.settled}%,7d   A* settled ${rAstar.settled}%,7d")
      println(f"  A* explored ${100.0 * rAstar.settled / rDij.settled}%.0f%% as many nodes, same cost: ${math.abs(rAstar.cost - rDij.cost) < 1e-6}")
    else println("  (source and target landed in different components; rerun with another seed)")

    rule("Alternative routes (Yen)")
    val alternatives = Yen.shortestPaths(g, source, target, 3)
    alternatives.zipWithIndex.foreach { (p, i) =>
      println(f"  route ${i + 1}  cost ${p.cost}%10.1f  hops ${p.hops}%4d")
    }
    println("  on a uniform grid an enormous number of routes tie for shortest,")
    println("  so Yen returns several genuinely distinct paths of identical cost")

    rule("Minimum spanning tree")
    val mst = Timed.print("Kruskal")(Kruskal.minimumSpanningForest(g))
    println(f"  edges ${mst.length}%,d  total weight ${mst.map(_.weight).sum}%,.0f m")

    rule("Where the heuristic stops helping")
    val maze = Generators.maze(60, 60, seed = 3)
    val mTarget = maze.numNodes - 1
    val mDij = Dijkstra.shortestPath(maze.graph, 0, mTarget)
    val mAstar = AStar.search(maze.graph, 0, mTarget, maze.heuristicTo(mTarget))
    println(f"  maze:  Dijkstra settled ${mDij.settled}%6d,  A* settled ${mAstar.settled}%6d")
    println("  a maze's corridors defeat straight-line distance -- the heuristic points through walls")

    val random = Generators.erdosRenyi(10000, 0.0006, seed = 2)
    val erDij = Dijkstra.shortestPath(random, 0, 9999)
    println(f"  random graph: no geometry at all, so no heuristic is possible (Dijkstra settled ${erDij.settled}%,d)")

  /** Routes on a real OSM extract and writes the result to GeoJSON. */
  def osmRoute(
      pbf: String,
      fromLat: Option[Double],
      fromLon: Option[Double],
      toLat: Option[Double],
      toLon: Option[Double],
      profile: TravelProfile,
      costModel: CostModel,
      out: Option[String]
  ): Unit =
    val (geo, stats) = OsmImporter.load(pbf, profile, costModel)
    println()
    println(stats)

    val (sw, ne) = geo.boundingBox
    println(s"\nBounding box: $sw to $ne")

    // Default to routing across the diagonal of the extract, which is a
    // reasonable long trip for any input.
    val from = (fromLat, fromLon) match
      case (Some(la), Some(lo)) => geo.nearestNode(LatLon(la, lo))
      case _                    => geo.nearestNode(sw)
    val to = (toLat, toLon) match
      case (Some(la), Some(lo)) => geo.nearestNode(LatLon(la, lo))
      case _                    => geo.nearestNode(ne)

    println(s"From junction $from ${geo.position(from)}")
    println(s"To   junction $to ${geo.position(to)}")

    val unit = costModel match
      case CostModel.Time     => "s"
      case CostModel.Distance => "m"

    rule("Routing")
    val dij = Timed.print("Dijkstra")(Dijkstra.shortestPath(geo.graph, from, to))
    val astar = Timed.print("A*")(AStar.search(geo.graph, from, to, geo.heuristicTo(to)))
    val rev = Timed.print("build reverse graph")(geo.graph.reverse)
    val bidi = Timed.print("bidirectional Dijkstra")(BidirectionalDijkstra.search(geo.graph, rev, from, to))

    dij.path match
      case None =>
        println("\nNo route found. If this persists, the extract may be split into disconnected pieces.")
      case Some(path) =>
        println()
        println(f"  cost     ${dij.cost}%.1f $unit")
        println(f"  length   ${geo.lengthMetres(path) / 1000}%.2f km over ${path.hops} road segments")
        println(f"  settled  Dijkstra ${dij.settled}%,d   A* ${astar.settled}%,d   bidirectional ${bidi.settled}%,d")
        if dij.settled > 0 then
          println(f"  A* explored ${100.0 * astar.settled / dij.settled}%.1f%% of what Dijkstra did")

        // The costs must agree; if they do not, the heuristic is inadmissible.
        val agree = math.abs(astar.cost - dij.cost) < 1e-6 && math.abs(bidi.cost - dij.cost) < 1e-6
        println(s"  all three algorithms agree on the cost: $agree")

        rule("Alternative routes (Yen, k=3)")
        val (alts, ms) = Timed("yen")(Yen.shortestPaths(geo.graph, from, to, 3))
        alts.zipWithIndex.foreach { (p, i) =>
          println(f"  route ${i + 1}  cost ${p.cost}%10.1f $unit  ${geo.lengthMetres(p) / 1000}%6.2f km  ${p.hops}%4d segments")
        }
        println(f"  (${ms}%.0f ms)")

        out.foreach { target =>
          val features = Seq(
            GeoJson.point(geo.position(from), Seq("marker" -> "start")),
            GeoJson.point(geo.position(to), Seq("marker" -> "end"))
          ) ++ alts.zipWithIndex.map((p, i) => GeoJson.path(geo, p, Seq("rank" -> (i + 1).toString)))
          val file = GeoJson.write(target, GeoJson.collection(features))
          println(s"\nWrote $file -- drop it on https://geojson.io to see the routes on a map")
        }

  /** Imports an extract and reports what came out, without routing. */
  def inspect(pbf: String, profile: TravelProfile, costModel: CostModel): Unit =
    val (geo, stats) = OsmImporter.load(pbf, profile, costModel)
    println()
    println(stats)

    val degrees = geo.graph.nodeIds.map(geo.graph.outDegree).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(_._1)
    rule("Out-degree distribution")
    degrees.take(12).foreach { (degree, count) =>
      val bar = "#" * math.min(50, (50.0 * count / geo.numNodes).ceil.toInt)
      println(f"  $degree%2d  $count%8d  $bar")
    }
    println("  a healthy road network is dominated by degree 1-4")

    val (sw, ne) = geo.boundingBox
    rule("Extent")
    println(s"  southwest $sw")
    println(s"  northeast $ne")
    println(f"  ${Haversine.distance(sw, LatLon(sw.lat, ne.lon)) / 1000}%.1f km wide, ${Haversine.distance(sw, LatLon(ne.lat, sw.lon)) / 1000}%.1f km tall")

  /** Compares algorithms over many random queries, with warm-up. */
  def bench(pbf: Option[String], queries: Int): Unit =
    val geo = pbf match
      case Some(path) =>
        val (g, stats) = OsmImporter.load(path, TravelProfile.Car, CostModel.Time)
        println(stats)
        g
      case None =>
        println("No --pbf given; benchmarking on a synthetic 300x300 grid instead")
        Generators.grid(300, 300)

    val g = geo.graph
    println(s"\n${g.numNodes} nodes, ${g.numEdges} edges, $queries random queries")

    val rev = Timed.print("build reverse graph")(g.reverse)
    val rng = scala.util.Random(1234)
    val pairs = Vector.fill(queries)((rng.nextInt(g.numNodes), rng.nextInt(g.numNodes)))

    // Warm-up: the JIT needs to compile the search loops before any timing
    // means anything. Skipping this can make the first algorithm look 10x
    // slower than it is.
    print("warming up JIT... ")
    pairs.take(math.min(20, queries)).foreach { (s, t) =>
      Dijkstra.shortestPath(g, s, t)
      AStar.search(g, s, t, geo.heuristicTo(t))
      BidirectionalDijkstra.search(g, rev, s, t)
    }
    println("done")

    var dijSettled = 0L
    var astarSettled = 0L
    var bidiSettled = 0L
    var found = 0

    val (_, dijMs) = Timed("d") {
      pairs.foreach { (s, t) =>
        val r = Dijkstra.shortestPath(g, s, t)
        dijSettled += r.settled
        if r.found then found += 1
      }
    }
    val (_, astarMs) = Timed("a") {
      pairs.foreach { (s, t) =>
        astarSettled += AStar.search(g, s, t, geo.heuristicTo(t)).settled
      }
    }
    val (_, bidiMs) = Timed("b") {
      pairs.foreach { (s, t) =>
        bidiSettled += BidirectionalDijkstra.search(g, rev, s, t).settled
      }
    }

    rule("Results")
    println(f"  ${"algorithm"}%-24s ${"total ms"}%10s ${"ms/query"}%10s ${"settled/query"}%15s")
    println(f"  ${"Dijkstra"}%-24s ${dijMs}%10.1f ${dijMs / queries}%10.2f ${dijSettled / queries}%15d")
    println(f"  ${"A*"}%-24s ${astarMs}%10.1f ${astarMs / queries}%10.2f ${astarSettled / queries}%15d")
    println(f"  ${"bidirectional Dijkstra"}%-24s ${bidiMs}%10.1f ${bidiMs / queries}%10.2f ${bidiSettled / queries}%15d")
    println(f"\n  $found of $queries pairs were connected")
    println("  note: A* is only worth it when the heuristic is informative -- compare these")
    println("  numbers against `demo tour`, which runs the same comparison on a maze.")

  /** Exports a search's settled set so the shape of each algorithm's frontier
    * can be seen on a map. This is the most convincing artifact in the project.
    */
  def visualize(out: String, size: Int): Unit =
    val geo = Generators.grid(size, size)
    val target = geo.numNodes - 1

    // No `stopAt`, so the search sweeps the whole grid and the settled set is
    // the full disc -- which is exactly what makes the picture worth looking at.
    val dijTree = Dijkstra.from(geo.graph, source = 0)
    val dijPath = dijTree.pathTo(target).get

    val features = Seq(
      GeoJson.point(geo.position(0), Seq("marker" -> "start")),
      GeoJson.point(geo.position(target), Seq("marker" -> "end")),
      GeoJson.path(geo, dijPath, Seq("algorithm" -> "dijkstra"))
    )
    val file = GeoJson.write(out, GeoJson.collection(features))
    println(s"Wrote $file")

    val settledFile = GeoJson.write(
      out.replace(".geojson", "-settled.geojson"),
      GeoJson.settledNodes(geo, dijTree, limit = 20000)
    )
    println(s"Wrote $settledFile -- every node Dijkstra settled")
    println("Open both on https://geojson.io: the settled set is a disc centred on the start.")
