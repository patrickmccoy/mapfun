package mapfun.age

import mapfun.geo.*

import java.sql.{Connection, DriverManager, PreparedStatement}
import scala.util.Using

/** Connection settings. Defaults match `docker-compose.yml` (port 5433 so this
  * never collides with a local Postgres).
  */
final case class AgeConfig(
    url: String = "jdbc:postgresql://localhost:5433/mapfun",
    user: String = "mapfun",
    password: String = "mapfun",
    graphName: String = "roads"
)

/** Apache AGE: openCypher running inside PostgreSQL.
  *
  * The interesting question AGE answers is whether you need a separate database
  * for graph work at all. Your data is already in Postgres; AGE lets you write
  * Cypher against it and join the results to ordinary tables in the same query,
  * which Neo4j fundamentally cannot do.
  *
  * The cost is ergonomics, and this class leans into that rather than hiding
  * it. Three things are unlike any other JDBC work:
  *
  *  1. Every session must `LOAD 'age'` and put `ag_catalog` on the search path.
  *     Connection pools hand out fresh sessions, so this has to happen per
  *     connection, not once at startup.
  *  2. Cypher is passed as a string to a `cypher()` function, and the result
  *     column types must be declared by the caller: `AS (v agtype)`.
  *  3. Everything comes back as `agtype`, a JSON-ish text format that you parse
  *     yourself. There is no driver-level object mapping.
  *
  * Written against plain JDBC on purpose. A query library would paper over
  * exactly the details worth seeing.
  */
final class AgeStore(config: AgeConfig):

  /** Opens a connection with the AGE preamble applied.
    *
    * `LOAD 'age'` loads the extension into the session, and putting
    * `ag_catalog` first on the search path is what makes the unqualified
    * `cypher(...)` and the `agtype` type resolve. Skip either and you get a
    * bare "function cypher(unknown, unknown) does not exist".
    */
  def connection(): Connection =
    val conn = DriverManager.getConnection(config.url, config.user, config.password)
    Using.resource(conn.createStatement()) { st =>
      st.execute("LOAD 'age'")
      st.execute("""SET search_path = ag_catalog, "$user", public""")
    }
    conn

  def withConnection[A](f: Connection => A): A =
    Using.resource(connection())(f)

  /** Installs the extension and creates the graph. Safe to re-run. */
  def createGraph(): Unit = withConnection { conn =>
    Using.resource(conn.createStatement()) { st =>
      st.execute("CREATE EXTENSION IF NOT EXISTS age")
      // create_graph throws if the graph exists, so check the catalog first --
      // there is no CREATE GRAPH IF NOT EXISTS.
      val exists = Using.resource(
        conn.prepareStatement("SELECT count(*) FROM ag_catalog.ag_graph WHERE name = ?")
      ) { ps =>
        ps.setString(1, config.graphName)
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          rs.getLong(1) > 0
        }
      }
      if !exists then st.execute(s"SELECT create_graph('${config.graphName}')")
    }
  }

  def dropGraph(): Unit = withConnection { conn =>
    Using.resource(conn.createStatement()) { st =>
      try st.execute(s"SELECT drop_graph('${config.graphName}', true)")
      catch case _: java.sql.SQLException => () // not there; nothing to drop
    }
  }

  /** Runs a Cypher statement that returns nothing.
    *
    * Even a write still needs a result column declared -- AGE requires the
    * `AS (v agtype)` clause regardless, which is why it is here rather than
    * being optional.
    *
    * Opens its own connection, which is fine for one-off calls but ruinous in
    * a loop: each one pays connection setup plus the `LOAD 'age'` preamble.
    * Bulk paths use [[executeOn]] against a connection they already hold.
    */
  def execute(cypher: String): Unit = withConnection(executeOn(_, cypher))

  private def executeOn(conn: Connection, cypher: String): Unit =
    Using.resource(conn.createStatement()) { st =>
      st.execute(ageQuery(cypher))
    }

  /** Runs Cypher and returns each row's single column as raw `agtype` text. */
  def query(cypher: String): Vector[String] = withConnection { conn =>
    Using.resource(conn.createStatement()) { st =>
      Using.resource(st.executeQuery(ageQuery(cypher))) { rs =>
        val out = Vector.newBuilder[String]
        while rs.next() do out += rs.getString(1)
        out.result()
      }
    }
  }

  private def ageQuery(cypher: String, columns: String = "v agtype"): String =
    // $$ ... $$ dollar-quoting avoids escaping every quote inside the Cypher.
    s"SELECT * FROM cypher('${config.graphName}', $$$$ $cypher $$$$) AS ($columns)"

  /** Bulk-loads a graph.
    *
    * AGE has no `UNWIND $rows`-style bulk path comparable to Neo4j's, and
    * generating one `CREATE` per row is painfully slow. The trick used here is
    * to build one statement containing many `CREATE` clauses, which amortises
    * the per-statement planning cost.
    *
    * Expect this to be substantially slower than the Neo4j load regardless.
    * That is a genuine finding about the two systems, not a flaw in the code --
    * AGE's write path goes through ordinary Postgres heap inserts.
    */
  def load(geo: GeoGraph, batchSize: Int = 200, verbose: Boolean = true): Unit =
    def log(msg: String): Unit = if verbose then println(s"[age] $msg")

    // One connection for the whole load. Opening one per statement would pay
    // the `LOAD 'age'` preamble thousands of times over.
    withConnection { conn =>
      log(s"loading ${geo.numNodes} junctions")
      geo.graph.nodeIds.grouped(batchSize).zipWithIndex.foreach { (batch, i) =>
        val creates = batch
          .map { n =>
            val p = geo.position(n)
            s"(:Junction {id: $n, lat: ${p.lat}, lon: ${p.lon}})"
          }
          .mkString("CREATE ", ", ", "")
        executeOn(conn, creates)
        if verbose && i % 5 == 4 then log(s"  junction batch ${i + 1}")
      }

      // Index only once the label table exists -- AGE creates it lazily with
      // the first vertex of that label -- and before the edge loading, which is
      // what actually needs the lookups.
      createIndexesOn(conn)

      val edges = for
        u <- geo.graph.nodeIds
        e <- geo.graph.edgeBegin(u) until geo.graph.edgeEnd(u)
      yield (u, geo.graph.target(e), geo.graph.weight(e))

      log(s"loading ${edges.length} roads")
      edges.zipWithIndex.foreach { case ((from, to, cost), i) =>
        // One MATCH pair plus one CREATE per edge. Verbose, but it is the shape
        // AGE actually supports -- there is no bulk relationship path here
        // comparable to Neo4j's UNWIND.
        executeOn(
          conn,
          s"MATCH (a:Junction {id: $from}), (b:Junction {id: $to}) CREATE (a)-[:ROAD {cost: $cost}]->(b)"
        )
        if verbose && i % 500 == 499 then log(s"  ${i + 1}/${edges.length} roads")
      }
      log("load complete")
    }

  /** Indexes the `id` property.
    *
    * AGE stores vertices in a Postgres table with the properties in a single
    * `agtype` column, so indexing a property means a *functional* index on the
    * extraction expression. Without it every `MATCH (n:Junction {id: 5})` is a
    * sequential scan of the whole label table -- and unlike Neo4j, nothing
    * warns you.
    */
  def createIndexes(): Unit = withConnection(createIndexesOn)

  private def createIndexesOn(conn: Connection): Unit =
    Using.resource(conn.createStatement()) { st =>
      val g = config.graphName
      try
        st.execute(s"""CREATE INDEX IF NOT EXISTS junction_id_idx
                       ON "$g"."Junction" (ag_catalog.agtype_access_operator(properties, '"id"'::agtype))""")
        st.execute(s"""ANALYZE "$g"."Junction"""")
      catch
        case e: java.sql.SQLException =>
          // The label table only exists once a vertex with that label has been
          // created, so on a fresh graph this is expected.
          println(s"[age] index not created (${e.getMessage.take(100)})")
    }

  def countJunctions(): Long =
    query("MATCH (j:Junction) RETURN count(j)").headOption.flatMap(Agtype.asLong).getOrElse(0L)

  def countRoads(): Long =
    query("MATCH ()-[r:ROAD]->() RETURN count(r)").headOption.flatMap(Agtype.asLong).getOrElse(0L)

  def neighbors(id: Int): Vector[Long] =
    query(s"MATCH (:Junction {id: $id})-[:ROAD]->(n:Junction) RETURN n.id")
      .flatMap(Agtype.asLong)

  /** Variable-length traversal. AGE supports the same `*1..n` syntax as Neo4j. */
  def withinHops(id: Int, hops: Int): Vector[Long] =
    query(s"MATCH (:Junction {id: $id})-[:ROAD*1..$hops]->(n:Junction) RETURN DISTINCT n.id")
      .flatMap(Agtype.asLong)

  /** Shortest path by hop count, via the same recursive traversal Cypher gives
    * you. AGE has no equivalent of Neo4j's GDS library, so weighted shortest
    * paths have to be written by hand -- see [[RecursiveCte]] for the SQL way
    * of doing it, which is the more interesting comparison anyway.
    */
  def pathByHops(from: Int, to: Int, maxHops: Int = 10): Option[Vector[Long]] =
    query(
      s"""MATCH p = (a:Junction {id: $from})-[:ROAD*1..$maxHops]->(b:Junction {id: $to})
          RETURN [n IN nodes(p) | n.id] LIMIT 1"""
    ).headOption.map(Agtype.asLongList)

/** A very small `agtype` reader.
  *
  * `agtype` is JSON with type suffixes: an integer comes back as `5`, a float
  * as `5.0`, and a vertex as `{"id": ..., "label": ..., "properties": {...}}::vertex`.
  * Only the fragments this project actually returns are handled -- enough to
  * see the shape of the format without writing a parser.
  */
object Agtype:

  /** Strips the `::vertex` / `::edge` / `::path` type annotation. */
  def stripSuffix(raw: String): String =
    val i = raw.lastIndexOf("::")
    if i >= 0 then raw.substring(0, i).trim else raw.trim

  def asLong(raw: String): Option[Long] =
    stripSuffix(raw).replace("\"", "").trim.toLongOption

  def asDouble(raw: String): Option[Double] =
    stripSuffix(raw).replace("\"", "").trim.toDoubleOption

  def asString(raw: String): String =
    val s = stripSuffix(raw)
    if s.length >= 2 && s.startsWith("\"") && s.endsWith("\"") then s.substring(1, s.length - 1)
    else s

  /** Reads a flat `[1, 2, 3]` array. */
  def asLongList(raw: String): Vector[Long] =
    val s = stripSuffix(raw)
    if !s.startsWith("[") then Vector.empty
    else
      s.stripPrefix("[")
        .stripSuffix("]")
        .split(',')
        .iterator
        .map(_.replace("\"", "").trim)
        .flatMap(_.toLongOption)
        .toVector

/** The same traversal expressed in pure SQL, with no graph extension at all.
  *
  * This is the honest comparison to make. A recursive common table expression
  * *is* a graph traversal -- `WITH RECURSIVE` walks the edge table exactly the
  * way BFS walks an adjacency list. Anyone claiming you need a graph database
  * to traverse a graph should be asked to explain this.
  *
  * What you give up is expressiveness: the Cypher version of the same query is
  * one line, and this is fifteen. What you gain is that it runs on any
  * Postgres, joins to your other tables for free, and uses the ordinary query
  * planner and indexes.
  *
  * These read from a plain relational `road_edges` table, deliberately separate
  * from the AGE graph, so the two models can be timed against each other on
  * identical data.
  */
final class RecursiveCte(config: AgeConfig):

  private def connection(): Connection =
    DriverManager.getConnection(config.url, config.user, config.password)

  /** Creates and fills an ordinary edge table. */
  def load(geo: GeoGraph, verbose: Boolean = true): Unit =
    Using.resource(connection()) { conn =>
      Using.resource(conn.createStatement()) { st =>
        st.execute("DROP TABLE IF EXISTS road_edges")
        st.execute("""CREATE TABLE road_edges (
                        from_id INTEGER NOT NULL,
                        to_id   INTEGER NOT NULL,
                        cost    DOUBLE PRECISION NOT NULL
                      )""")
        st.execute("""CREATE TABLE IF NOT EXISTS road_nodes (
                        id  INTEGER PRIMARY KEY,
                        lat DOUBLE PRECISION NOT NULL,
                        lon DOUBLE PRECISION NOT NULL
                      )""")
        st.execute("TRUNCATE road_nodes")
      }

      conn.setAutoCommit(false)
      Using.resource(conn.prepareStatement("INSERT INTO road_nodes VALUES (?, ?, ?)")) { ps =>
        geo.graph.nodeIds.foreach { n =>
          ps.setInt(1, n)
          ps.setDouble(2, geo.position(n).lat)
          ps.setDouble(3, geo.position(n).lon)
          ps.addBatch()
        }
        ps.executeBatch()
      }
      Using.resource(conn.prepareStatement("INSERT INTO road_edges VALUES (?, ?, ?)")) { ps =>
        geo.graph.nodeIds.foreach { u =>
          (geo.graph.edgeBegin(u) until geo.graph.edgeEnd(u)).foreach { e =>
            ps.setInt(1, u)
            ps.setInt(2, geo.graph.target(e))
            ps.setDouble(3, geo.graph.weight(e))
            ps.addBatch()
          }
        }
        ps.executeBatch()
      }
      conn.commit()
      conn.setAutoCommit(true)

      // The index that makes the recursive step a lookup rather than a scan.
      // Without it the CTE below degrades to a full table scan per level.
      Using.resource(conn.createStatement()) { st =>
        st.execute("CREATE INDEX IF NOT EXISTS road_edges_from_idx ON road_edges (from_id)")
        st.execute("ANALYZE road_edges")
      }
      if verbose then println(s"[cte] loaded ${geo.numEdges} edges into road_edges")
    }

  /** Everything reachable within `maxHops`, as a breadth-first expansion.
    *
    * `UNION` rather than `UNION ALL` is doing the visited-set deduplication
    * that BFS would do with a boolean array. With `UNION ALL` this never
    * terminates on a cyclic graph.
    */
  def withinHops(from: Int, maxHops: Int): Vector[(Int, Int)] =
    val sql = """
      WITH RECURSIVE reachable(id, depth) AS (
          SELECT ?::integer, 0
        UNION
          SELECT e.to_id, r.depth + 1
          FROM reachable r
          JOIN road_edges e ON e.from_id = r.id
          WHERE r.depth < ?
      )
      SELECT id, min(depth) FROM reachable GROUP BY id ORDER BY min(depth), id
    """
    Using.resource(connection()) { conn =>
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setInt(1, from)
        ps.setInt(2, maxHops)
        readPairs(ps)
      }
    }

  /** Weighted shortest path costs, by relaxation in SQL.
    *
    * This is Bellman-Ford, not Dijkstra: a recursive CTE has no priority queue,
    * so it cannot settle nodes in cost order. It explores every path under the
    * hop limit and takes the minimum, which is why the depth bound is
    * mandatory -- without it, this does not terminate on a cyclic graph.
    *
    * Expect it to be dramatically slower than `core`'s Dijkstra. Being able to
    * see exactly *why*, in terms of the algorithm the query is forced to run,
    * is the point of having it here.
    */
  def shortestCosts(from: Int, maxHops: Int = 8): Vector[(Int, Double)] =
    val sql = """
      WITH RECURSIVE walk(id, cost, depth) AS (
          SELECT ?::integer, 0.0::double precision, 0
        UNION ALL
          SELECT e.to_id, w.cost + e.cost, w.depth + 1
          FROM walk w
          JOIN road_edges e ON e.from_id = w.id
          WHERE w.depth < ?
      )
      SELECT id, min(cost) FROM walk GROUP BY id ORDER BY min(cost)
    """
    Using.resource(connection()) { conn =>
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setInt(1, from)
        ps.setInt(2, maxHops)
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[(Int, Double)]
          while rs.next() do out += ((rs.getInt(1), rs.getDouble(2)))
          out.result()
        }
      }
    }

  private def readPairs(ps: PreparedStatement): Vector[(Int, Int)] =
    Using.resource(ps.executeQuery()) { rs =>
      val out = Vector.newBuilder[(Int, Int)]
      while rs.next() do out += ((rs.getInt(1), rs.getInt(2)))
      out.result()
    }
