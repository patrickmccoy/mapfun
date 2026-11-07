package mapfun.cli

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp

import mapfun.geo.GeoGraph
import mapfun.osm.{OsmImporter, TravelProfile, CostModel}
import mapfun.neo4j.{Neo4jConfig, Neo4jStore, GraphDataScience}
import mapfun.age.{AgeConfig, AgeStore, RecursiveCte}

/** Entry point. Run subcommands with:
  *
  * {{{
  *   sbt "cli/run demo"
  *   sbt "cli/run route --pbf data/monaco-latest.osm.pbf --out out/route.geojson"
  * }}}
  */
object Main
    extends CommandIOApp(
      name = "mapfun",
      header = "A playground for map routing algorithms and graph databases.",
      version = "0.1.0"
    ):

  // ---- shared options ------------------------------------------------------

  private val pbfOpt = Opts
    .option[String]("pbf", "Path to an .osm.pbf extract (see scripts/fetch-osm.sh).")

  private val profileOpt = Opts
    .option[String]("profile", "Travel profile: car, foot or bike. Default car.")
    .withDefault("car")
    .mapValidated {
      case "car"  => TravelProfile.Car.validNel
      case "foot" => TravelProfile.Foot.validNel
      case "bike" => TravelProfile.Bike.validNel
      case other  => s"unknown profile '$other' (expected car, foot or bike)".invalidNel
    }

  private val costOpt = Opts
    .option[String]("cost", "Edge cost: time or distance. Default time.")
    .withDefault("time")
    .mapValidated {
      case "time"     => CostModel.Time.validNel
      case "distance" => CostModel.Distance.validNel
      case other      => s"unknown cost model '$other' (expected time or distance)".invalidNel
    }

  private val outOpt = Opts.option[String]("out", "Write GeoJSON here.").orNone

  private val neo4jConfig = (
    Opts.option[String]("neo4j-uri", "Bolt URI.").withDefault("bolt://localhost:7687"),
    Opts.option[String]("neo4j-user", "Username.").withDefault("neo4j"),
    Opts.option[String]("neo4j-password", "Password.").withDefault("mapfunmapfun")
  ).mapN(Neo4jConfig.apply)

  private val ageConfig = (
    Opts.option[String]("age-url", "JDBC URL.").withDefault("jdbc:postgresql://localhost:5433/mapfun"),
    Opts.option[String]("age-user", "Username.").withDefault("mapfun"),
    Opts.option[String]("age-password", "Password.").withDefault("mapfun"),
    Opts.option[String]("age-graph", "Graph name.").withDefault("roads")
  ).mapN(AgeConfig.apply)

  /** Keeps database loads to a manageable size. Pushing a million junctions
    * through Bolt one batch at a time is not the lesson; seeing the query
    * models is.
    */
  private val limitOpt = Opts
    .option[Int]("limit", "Cap the graph to this many junctions before loading. Default 20000.")
    .withDefault(20000)

  /** The slice of an import that the `load-*` commands store: the first `n`
    * junction ids. Everything that later compares against a database has to
    * rebuild the same slice, or it is comparing two different graphs.
    */
  private def firstJunctions(geo: GeoGraph, n: Int): GeoGraph =
    if geo.numNodes > n then geo.filter(_ < n) else geo

  // ---- subcommands ---------------------------------------------------------

  private val demo = Opts.subcommand("demo", "Run the algorithm tour on synthetic graphs.") {
    Opts.unit.map(_ => IO(Demos.tour()))
  }

  private val inspect = Opts.subcommand("inspect", "Import an extract and describe what came out.") {
    (pbfOpt, profileOpt, costOpt).mapN { (pbf, profile, cost) =>
      IO(Demos.inspect(pbf, profile, cost))
    }
  }

  private val route = Opts.subcommand("route", "Route across a real OSM extract.") {
    (
      pbfOpt,
      Opts.option[Double]("from-lat", "Start latitude.").orNone,
      Opts.option[Double]("from-lon", "Start longitude.").orNone,
      Opts.option[Double]("to-lat", "End latitude.").orNone,
      Opts.option[Double]("to-lon", "End longitude.").orNone,
      profileOpt,
      costOpt,
      outOpt
    ).mapN { (pbf, fLat, fLon, tLat, tLon, profile, cost, out) =>
      IO(Demos.osmRoute(pbf, fLat, fLon, tLat, tLon, profile, cost, out))
    }
  }

  private val bench = Opts.subcommand("bench", "Compare algorithms over many random queries.") {
    (
      Opts.option[String]("pbf", "Extract to benchmark on. Omitted: a synthetic grid.").orNone,
      Opts.option[Int]("queries", "How many random queries. Default 200.").withDefault(200)
    ).mapN { (pbf, queries) => IO(Demos.bench(pbf, queries)) }
  }

  private val visualize = Opts.subcommand("visualize", "Write a route and its settled set as GeoJSON.") {
    (
      Opts.option[String]("out", "Output path.").withDefault("out/search.geojson"),
      Opts.option[Int]("size", "Grid side length. Default 80.").withDefault(80)
    ).mapN { (out, size) => IO(Demos.visualize(out, size)) }
  }

  private val loadNeo4j = Opts.subcommand("load-neo4j", "Import an extract into Neo4j.") {
    (pbfOpt, profileOpt, costOpt, limitOpt, neo4jConfig).mapN { (pbf, profile, cost, limit, config) =>
      val (full, stats) = OsmImporter.load(pbf, profile, cost)
      val geo = firstJunctions(full, limit)
      Neo4jStore.resource(config).use { store =>
        for
          _ <- IO.println(stats.toString)
          _ <- IO.println(s"loading ${geo.numNodes} junctions into Neo4j at ${config.uri}")
          _ <- store.createSchema
          _ <- store.clear
          _ <- store.load(geo)
          n <- store.countJunctions
          r <- store.countRoads
          _ <- IO.println(s"Neo4j now holds $n junctions and $r roads")
          d <- store.degreeDistribution
          _ <- IO.println("degree distribution: " + d.take(8).map((k, v) => s"$k->$v").mkString(", "))
        yield ()
      }
    }
  }

  private val queryNeo4j = Opts.subcommand("neo4j", "Run Cypher and GDS against a loaded Neo4j graph.") {
    (
      Opts.option[Long]("from", "Source junction id.").withDefault(0L),
      Opts.option[Long]("to", "Target junction id.").withDefault(100L),
      Opts.flag("gds", "Also project the graph and run GDS algorithms.").orFalse,
      Opts.option[String]("pbf", "Same extract used to load, to cross-check GDS against core.").orNone,
      profileOpt,
      costOpt,
      neo4jConfig
    ).mapN { (from, to, gds, pbf, profile, cost, config) =>
      Neo4jStore.resource(config).use { store =>
        for
          n <- store.countJunctions
          _ <- IO.println(s"$n junctions in the database")
          nb <- store.neighbors(from)
          _ <- IO.println(s"neighbours of $from: ${nb.mkString(", ")}")
          reach <- store.withinHops(from, 3)
          _ <- IO.println(s"within 3 hops of $from: ${reach.length} junctions")
          hops <- store.shortestPathByHops(from, to)
          _ <- IO.println(s"fewest-hops path $from -> $to: ${hops.map(_.mkString(" -> ")).getOrElse("none")}")
          _ <- IO.whenA(gds)(runGds(config, from, to, pbf, profile, cost, loaded = n))
        yield ()
      }
    }
  }

  private def runGds(
      config: Neo4jConfig,
      from: Long,
      to: Long,
      pbf: Option[String],
      profile: TravelProfile,
      cost: CostModel,
      loaded: Int
  ): IO[Unit] =
    GraphDataScience.resource(config).use { gds =>
      for
        _ <- IO.println("\n-- Graph Data Science --")
        count <- gds.project()
        _ <- IO.println(s"projected $count nodes into an in-memory graph")
        d <- gds.dijkstra(from, to)
        _ <- IO.println(s"gds Dijkstra: ${describe(d)}")
        a <- gds.aStar(from, to)
        _ <- IO.println(s"gds A*:       ${describe(a)}")
        y <- gds.yens(from, to, 3)
        _ <- IO.println(s"gds Yen k=3:  ${y.map(p => f"${p._1}%.1f").mkString(", ")}")
        b <- gds.betweenness(limit = 5)
        _ <- IO.println("most central junctions: " + b.map((id, s) => f"$id ($s%.0f)").mkString(", "))
        _ <- pbf.traverse_(p => crossCheck(p, profile, cost, loaded, from, to, d.map(_._1)))
      yield ()
    }

  /** Re-imports the extract in memory and compares `core`'s Dijkstra with what
    * GDS returned.
    *
    * The strongest single test in the project: it exercises the importer, the
    * graph build, the search, the Bolt load and the GDS projection at once, and
    * two completely independent implementations have to land on the same number.
    * Only meaningful when the same `--profile` and `--cost` were used to load.
    *
    * `load-neo4j` stores only the first `--limit` junctions, so the full
    * re-import is cut down to the same `loaded` count first. Comparing against
    * the whole graph would check two different networks and report a mismatch
    * that has nothing to do with either implementation.
    */
  private def crossCheck(
      pbf: String,
      profile: TravelProfile,
      cost: CostModel,
      loaded: Int,
      from: Long,
      to: Long,
      gdsCost: Option[Double]
  ): IO[Unit] = IO {
    val (full, _) = OsmImporter.load(pbf, profile, cost, verbose = false)
    val geo = firstJunctions(full, loaded)
    println("\n-- core vs GDS --")
    if full.numNodes < loaded then
      println(s"  the database holds $loaded junctions but this extract only produces ${full.numNodes}:")
      println("  it was loaded from a different --pbf, --profile or --cost, so there is nothing to compare")
    else if from < 0 || to < 0 || from >= geo.numNodes || to >= geo.numNodes then
      println(s"  --from/--to must be junction ids below ${geo.numNodes}")
    else
      compareWithGds(geo, from.toInt, to.toInt, gdsCost)
  }

  private def compareWithGds(geo: GeoGraph, from: Int, to: Int, gdsCost: Option[Double]): Unit =
    val local = mapfun.algo.Dijkstra.shortestPath(geo.graph, from, to)
    println(f"  core Dijkstra: ${local.cost}%.3f  (settled ${local.settled})")
    gdsCost match
      case Some(remote) =>
        println(f"  gds  Dijkstra: ${remote}%.3f")
        val agree = math.abs(remote - local.cost) < 1e-3
        println(s"  they agree: $agree")
        if !agree then
          println("  (if this is false, the database was loaded with a different --profile/--cost)")
      case None =>
        // The loaded slice is an id prefix, not a strongly connected region,
        // so "no path" is a real answer here, and core should agree.
        println("  gds returned no path")
        println(s"  core ${if !local.found then "agrees" else "found one, so they disagree"}")

  private def describe(r: Option[(Double, List[Long])]): String =
    r.fold("no path")((cost, nodes) => f"cost $cost%.1f over ${nodes.length} junctions")

  private val loadAge = Opts.subcommand("load-age", "Import an extract into Apache AGE and a plain SQL table.") {
    (pbfOpt, profileOpt, costOpt, limitOpt, ageConfig).mapN { (pbf, profile, cost, limit, config) =>
      IO {
        val (full, stats) = OsmImporter.load(pbf, profile, cost)
        // AGE's write path is much slower than Neo4j's, so default to a
        // smaller slice unless the user asks for more.
        val cap = math.min(limit, 5000)
        val geo = firstJunctions(full, cap)
        println(stats)
        println(s"loading ${geo.numNodes} junctions into AGE at ${config.url}")

        val store = AgeStore(config)
        store.dropGraph()
        store.createGraph()
        store.load(geo)
        println(s"AGE now holds ${store.countJunctions()} junctions and ${store.countRoads()} roads")

        // The same data as ordinary rows, for the SQL comparison.
        RecursiveCte(config).load(geo)
      }
    }
  }

  private val queryAge = Opts.subcommand("age", "Compare Cypher-on-Postgres with a recursive CTE.") {
    (
      Opts.option[Int]("from", "Source junction id.").withDefault(0),
      Opts.option[Int]("hops", "Traversal depth. Default 3.").withDefault(3),
      ageConfig
    ).mapN { (from, hops, config) =>
      IO {
        val store = AgeStore(config)
        println(s"AGE holds ${store.countJunctions()} junctions and ${store.countRoads()} roads")

        println("\n-- openCypher via AGE --")
        val (cypherResult, cypherMs) = Timed("age")(store.withinHops(from, hops))
        println(f"  within $hops hops of $from: ${cypherResult.length} junctions (${cypherMs}%.0f ms)")

        println("\n-- the same traversal as a recursive CTE, no graph extension --")
        val cte = RecursiveCte(config)
        val (cteResult, cteMs) = Timed("cte")(cte.withinHops(from, hops))
        println(f"  within $hops hops of $from: ${cteResult.length} junctions (${cteMs}%.0f ms)")

        println("\n-- weighted costs by relaxation in SQL (Bellman-Ford, not Dijkstra) --")
        val (costs, costMs) = Timed("costs")(cte.shortestCosts(from, hops))
        println(f"  reached ${costs.length} junctions (${costMs}%.0f ms)")
        costs.take(5).foreach((id, c) => println(f"    junction $id%6d  cost ${c}%10.1f"))

        // The CTE seeds itself with the source at depth 0, so it always
        // contains it. The Cypher pattern `-[:ROAD*1..n]->` requires at least
        // one hop, so it contains the source only when some cycle leads back
        // there. Add it to both sides before comparing.
        val viaCypher = cypherResult.map(_.toInt).toSet + from
        val viaCte = cteResult.map(_._1).toSet + from
        println(s"\n  both approaches reached the same ${viaCte.size} junctions: ${viaCypher == viaCte}")
        if viaCypher != viaCte then
          println(s"    only via Cypher: ${(viaCypher -- viaCte).toSeq.sorted.mkString(", ")}")
          println(s"    only via CTE:    ${(viaCte -- viaCypher).toSeq.sorted.mkString(", ")}")
      }
    }
  }

  def main: Opts[IO[ExitCode]] =
    val commands =
      demo orElse inspect orElse route orElse bench orElse visualize orElse
        loadNeo4j orElse queryNeo4j orElse loadAge orElse queryAge

    // Turn the inevitable "database is not running" into a readable message
    // rather than a stack trace, since half these subcommands need Docker up.
    commands.map { action =>
      action.as(ExitCode.Success).handleErrorWith { error =>
        IO.println(s"\nFailed: ${error.getClass.getSimpleName}: ${error.getMessage}")
          .as(ExitCode.Error)
      }
    }
