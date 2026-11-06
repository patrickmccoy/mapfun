package mapfun.neo4j

import cats.effect.{IO, Resource}
import cats.syntax.all.*

import neotypes.{AsyncDriver, GraphDatabase}
import neotypes.mappers.ResultMapper
import neotypes.syntax.all.*
import neotypes.cats.effect.implicits.*

import org.neo4j.driver.AuthTokens

import mapfun.geo.*

/** Connection settings. Defaults match `docker-compose.yml`. */
final case class Neo4jConfig(
    uri: String = "bolt://localhost:7687",
    user: String = "neo4j",
    password: String = "mapfunmapfun"
)

/** The road network as a Neo4j property graph.
  *
  * Worth being clear about what this buys you, because it is not speed. The
  * in-memory [[mapfun.algo.Dijkstra]] in `core` will beat any round trip to a
  * database, every time. What a graph database gives you is a *queryable*
  * graph: pattern matching over structure, ad-hoc traversals you did not write
  * an algorithm for, and persistence across restarts.
  *
  * The mapping used here:
  *
  * {{{
  *   (:Junction {id, lat, lon})-[:ROAD {cost, metres}]->(:Junction)
  * }}}
  *
  * One node label, one relationship type. Relationships in Neo4j are always
  * directed and always traversable in either direction at query time, so a
  * one-way street is a single `ROAD` and a two-way street is two.
  */
final class Neo4jStore(driver: AsyncDriver[IO]):

  /** Constraints and indexes.
    *
    * The unique constraint on `Junction.id` is not decoration -- it creates the
    * backing index that makes `MATCH (j:Junction {id: $id})` a lookup instead of
    * a scan of every node in the database. Forgetting it is the single most
    * common reason a Neo4j import or query is inexplicably slow.
    */
  def createSchema: IO[Unit] =
    for
      _ <- c"CREATE CONSTRAINT junction_id IF NOT EXISTS FOR (j:Junction) REQUIRE j.id IS UNIQUE".execute
             .void(driver)
      _ <- c"CREATE INDEX junction_location IF NOT EXISTS FOR (j:Junction) ON (j.lat, j.lon)".execute
             .void(driver)
    yield ()

  def clear: IO[Unit] =
    // CALL { ... } IN TRANSACTIONS keeps a large delete from building one
    // enormous transaction and exhausting the heap.
    c"MATCH (j:Junction) CALL { WITH j DETACH DELETE j } IN TRANSACTIONS OF 10000 ROWS".execute
      .void(driver)

  /** Bulk-loads a graph.
    *
    * Batched with `UNWIND` rather than one statement per row. This is the
    * difference between an import that takes minutes and one that takes hours:
    * each round trip carries thousands of rows, and the query is planned once
    * and reused instead of being re-parsed per node.
    */
  def load(geo: GeoGraph, batchSize: Int = 5000, verbose: Boolean = true): IO[Unit] =
    def log(msg: String): IO[Unit] = IO.whenA(verbose)(IO.println(s"[neo4j] $msg"))

    val nodeBatches = geo.graph.nodeIds
      .map(n => (n.toLong, geo.position(n).lat, geo.position(n).lon))
      .grouped(batchSize)
      .toVector

    val edgeRows = for
      u <- geo.graph.nodeIds
      e <- geo.graph.edgeBegin(u) until geo.graph.edgeEnd(u)
    yield (u.toLong, geo.graph.target(e).toLong, geo.graph.weight(e))

    val edgeBatches = edgeRows.grouped(batchSize).toVector

    for
      _ <- log(s"loading ${geo.numNodes} junctions in ${nodeBatches.size} batches")
      _ <- nodeBatches.zipWithIndex.traverse_ { (batch, i) =>
             val ids = batch.map(_._1).toList
             val lats = batch.map(_._2).toList
             val lons = batch.map(_._3).toList
             c"""UNWIND range(0, size($ids) - 1) AS i
                 MERGE (j:Junction {id: $ids[i]})
                 SET j.lat = $lats[i], j.lon = $lons[i]""".execute
               .void(driver)
               .flatTap(_ => IO.whenA(verbose && i % 20 == 19)(IO.println(s"[neo4j]   node batch ${i + 1}/${nodeBatches.size}")))
           }
      _ <- log(s"loading ${edgeRows.length} roads in ${edgeBatches.size} batches")
      _ <- edgeBatches.zipWithIndex.traverse_ { (batch, i) =>
             val froms = batch.map(_._1).toList
             val tos = batch.map(_._2).toList
             val costs = batch.map(_._3).toList
             c"""UNWIND range(0, size($froms) - 1) AS i
                 MATCH (a:Junction {id: $froms[i]})
                 MATCH (b:Junction {id: $tos[i]})
                 CREATE (a)-[:ROAD {cost: $costs[i]}]->(b)""".execute
               .void(driver)
               .flatTap(_ => IO.whenA(verbose && i % 20 == 19)(IO.println(s"[neo4j]   edge batch ${i + 1}/${edgeBatches.size}")))
           }
      _ <- log("load complete")
    yield ()

  def countJunctions: IO[Int] =
    c"MATCH (j:Junction) RETURN count(j)".query(ResultMapper.int).single(driver)

  def countRoads: IO[Int] =
    c"MATCH ()-[r:ROAD]->() RETURN count(r)".query(ResultMapper.int).single(driver)

  /** Neighbours of a junction -- the "hello world" of a graph database, and a
    * good way to confirm the load worked.
    */
  def neighbors(id: Long): IO[List[(Long, Double)]] =
    c"""MATCH (:Junction {id: $id})-[r:ROAD]->(n:Junction)
        RETURN n.id, r.cost"""
      .query(ResultMapper.tuple[Long, Double])
      .list(driver)

  /** Everything within `hops` relationships, using Cypher's variable-length
    * pattern.
    *
    * `[:ROAD*1..3]` is the feature that has no clean SQL equivalent, and the
    * reason people reach for a graph database. Note it counts *hops*, not
    * distance -- three roads could be thirty metres or thirty kilometres.
    * Bounding the length is essential; an unbounded `*` on a road network will
    * try to enumerate an astronomical number of paths.
    */
  def withinHops(id: Long, hops: Int): IO[List[Long]] =
    c"""MATCH (start:Junction {id: $id})-[:ROAD*1..#$hops]->(n:Junction)
        RETURN DISTINCT n.id"""
      .query(ResultMapper.long)
      .list(driver)

  /** Shortest path by hop count, using Cypher's built-in `shortestPath`.
    *
    * Careful: this minimises the *number of relationships*, ignoring `cost`
    * entirely. On a road network that is the route with the fewest junctions,
    * which is rarely the fastest or shortest one. For weighted routing you want
    * [[GraphDataScience.dijkstra]].
    */
  def shortestPathByHops(from: Long, to: Long): IO[Option[List[Long]]] =
    c"""MATCH (a:Junction {id: $from}), (b:Junction {id: $to})
        MATCH p = shortestPath((a)-[:ROAD*..100]->(b))
        RETURN [n IN nodes(p) | n.id]"""
      .query(ResultMapper.list(ResultMapper.long))
      .list(driver)
      .map(_.headOption)

  /** Degree distribution -- an easy sanity check on an import.
    *
    * A road network should be overwhelmingly degree 2-4. A big spike anywhere
    * else usually means the junction detection went wrong.
    */
  def degreeDistribution: IO[List[(Int, Int)]] =
    c"""MATCH (j:Junction)
        WITH j, count { (j)-[:ROAD]->() } AS degree
        RETURN degree, count(j) AS junctions
        ORDER BY degree"""
      .query(ResultMapper.tuple[Int, Int])
      .list(driver)

  /** Runs an arbitrary Cypher statement returning one string column.
    *
    * Deliberately included so you can experiment from the CLI without
    * recompiling. Note the query is interpolated as a literal -- fine for a
    * local playground, absolutely not a pattern to carry into anything
    * handling untrusted input.
    */
  def rawQuery(cypher: String): IO[List[String]] =
    c"#$cypher".query(ResultMapper.string).list(driver)

object Neo4jStore:

  /** Opens a driver as a `Resource`, so the connection pool is always closed --
    * including when the program fails partway through.
    */
  def resource(config: Neo4jConfig = Neo4jConfig()): Resource[IO, Neo4jStore] =
    GraphDatabase
      .asyncDriver[IO](config.uri, AuthTokens.basic(config.user, config.password))
      .map(new Neo4jStore(_))

/** Neo4j's Graph Data Science library: the algorithms from `core`, implemented
  * inside the database.
  *
  * The model is two-phase, and the first phase is the interesting one. You
  * *project* a graph into an in-memory representation, and algorithms run
  * against that projection rather than against stored nodes and relationships.
  * That projection is essentially the same CSR structure `core` builds -- GDS
  * is doing exactly what this project does, just behind a procedure call.
  *
  * Projections are held in memory and survive until dropped or the database
  * restarts, which is why every method here is careful about lifecycle.
  */
final class GraphDataScience(driver: AsyncDriver[IO]):

  /** Projects `Junction`/`ROAD` into a named in-memory graph.
    *
    * Both the relationship weight *and* the node coordinates have to be named
    * here. A projection carries only what you ask for, so leaving out
    * `sourceNodeProperties`/`targetNodeProperties` produces a graph on which
    * Dijkstra works fine and [[aStar]] fails with "The property `lat` has not
    * been loaded" -- the projection, not the stored data, is what the
    * algorithms see.
    */
  def project(name: String = "roads"): IO[Long] =
    for
      _ <- drop(name)
      count <- c"""MATCH (source:Junction)-[r:ROAD]->(target:Junction)
                   RETURN gds.graph.project($name, source, target, {
                     sourceNodeProperties: source { .lat, .lon },
                     targetNodeProperties: target { .lat, .lon },
                     relationshipProperties: r { .cost }
                   }).nodeCount"""
                 .query(ResultMapper.long)
                 .single(driver)
    yield count

  def drop(name: String = "roads"): IO[Unit] =
    c"CALL gds.graph.drop($name, false) YIELD graphName".execute.void(driver)

  def exists(name: String = "roads"): IO[Boolean] =
    c"CALL gds.graph.exists($name) YIELD exists RETURN exists"
      .query(ResultMapper.boolean)
      .single(driver)

  /** Weighted shortest path -- the real equivalent of `core`'s Dijkstra. */
  def dijkstra(from: Long, to: Long, graphName: String = "roads"): IO[Option[(Double, List[Long])]] =
    c"""MATCH (s:Junction {id: $from}), (t:Junction {id: $to})
        CALL gds.shortestPath.dijkstra.stream($graphName, {
          sourceNode: s, targetNode: t, relationshipWeightProperty: 'cost'
        })
        YIELD totalCost, nodeIds
        RETURN totalCost, [n IN nodeIds | gds.util.asNode(n).id]"""
      .query(ResultMapper.tuple[Double, List[Long]])
      .list(driver)
      .map(_.headOption)

  /** A* -- GDS wants the coordinate properties by name so it can compute the
    * same haversine heuristic `core` computes.
    */
  def aStar(from: Long, to: Long, graphName: String = "roads"): IO[Option[(Double, List[Long])]] =
    c"""MATCH (s:Junction {id: $from}), (t:Junction {id: $to})
        CALL gds.shortestPath.astar.stream($graphName, {
          sourceNode: s, targetNode: t,
          latitudeProperty: 'lat', longitudeProperty: 'lon',
          relationshipWeightProperty: 'cost'
        })
        YIELD totalCost, nodeIds
        RETURN totalCost, [n IN nodeIds | gds.util.asNode(n).id]"""
      .query(ResultMapper.tuple[Double, List[Long]])
      .list(driver)
      .map(_.headOption)

  /** Yen's k-shortest paths -- the same algorithm as `core`'s [[mapfun.algo.Yen]]. */
  def yens(from: Long, to: Long, k: Int, graphName: String = "roads"): IO[List[(Double, List[Long])]] =
    c"""MATCH (s:Junction {id: $from}), (t:Junction {id: $to})
        CALL gds.shortestPath.yens.stream($graphName, {
          sourceNode: s, targetNode: t, k: $k, relationshipWeightProperty: 'cost'
        })
        YIELD totalCost, nodeIds
        RETURN totalCost, [n IN nodeIds | gds.util.asNode(n).id]"""
      .query(ResultMapper.tuple[Double, List[Long]])
      .list(driver)

  /** Weakly connected components. Compare with `core`'s [[mapfun.algo.Tarjan]],
    * which computes the *strongly* connected ones -- on a one-way road network
    * the two give noticeably different answers.
    */
  def weaklyConnectedComponents(graphName: String = "roads"): IO[List[(Long, Long)]] =
    c"""CALL gds.wcc.stream($graphName)
        YIELD componentId
        RETURN componentId, count(*) AS size
        ORDER BY size DESC LIMIT 10"""
      .query(ResultMapper.tuple[Long, Long])
      .list(driver)

  /** Betweenness centrality: how many shortest paths run through each junction.
    *
    * There is no equivalent in `core`, and it is a good example of a question
    * routing algorithms do not answer but network analysis does -- the top
    * results are the bridges and arterial roads a city cannot function without.
    */
  def betweenness(graphName: String = "roads", limit: Int = 10): IO[List[(Long, Double)]] =
    c"""CALL gds.betweenness.stream($graphName)
        YIELD nodeId, score
        RETURN gds.util.asNode(nodeId).id AS id, score
        ORDER BY score DESC LIMIT $limit"""
      .query(ResultMapper.tuple[Long, Double])
      .list(driver)

  /** PageRank over the road network. Interpret with care: it was designed for
    * hyperlinks, and "important junction" is not quite what it measures here.
    */
  def pageRank(graphName: String = "roads", limit: Int = 10): IO[List[(Long, Double)]] =
    c"""CALL gds.pageRank.stream($graphName)
        YIELD nodeId, score
        RETURN gds.util.asNode(nodeId).id AS id, score
        ORDER BY score DESC LIMIT $limit"""
      .query(ResultMapper.tuple[Long, Double])
      .list(driver)

object GraphDataScience:
  def resource(config: Neo4jConfig = Neo4jConfig()): Resource[IO, GraphDataScience] =
    GraphDatabase
      .asyncDriver[IO](config.uri, AuthTokens.basic(config.user, config.password))
      .map(new GraphDataScience(_))
