package mapfun.io

import mapfun.graph.*
import mapfun.geo.*
import java.nio.file.{Files, Path as FsPath, Paths}
import java.nio.charset.StandardCharsets

/** Minimal GeoJSON writer, so results can actually be looked at.
  *
  * Drag the output onto https://geojson.io (or open it in QGIS) and the route
  * appears on a real basemap. This matters more than it sounds: a shortest path
  * that cuts straight through a river is instantly obvious on a map and
  * invisible in a list of node ids.
  *
  * Hand-rolled rather than pulling in a JSON library -- the output shape is
  * fixed and tiny, and it keeps `core` dependency-free.
  */
object GeoJson:

  private def escape(s: String): String =
    val sb = StringBuilder()
    s.foreach {
      case '"'  => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c    => sb += c
    }
    sb.result()

  private def coord(p: LatLon): String =
    // GeoJSON is [longitude, latitude] -- the reverse of how everyone says it
    // out loud, and a reliable source of maps that render in the ocean.
    f"[${p.lon}%.7f,${p.lat}%.7f]"

  private def props(pairs: Seq[(String, String)]): String =
    pairs.map((k, v) => s""""${escape(k)}":"${escape(v)}"""").mkString("{", ",", "}")

  /** A route as a LineString. */
  def path(geo: GeoGraph, route: Path, properties: Seq[(String, String)] = Seq.empty): String =
    val coords = route.nodes.map(n => coord(geo.position(n))).mkString("[", ",", "]")
    val allProps = properties ++ Seq(
      "cost" -> f"${route.cost}%.3f",
      "hops" -> route.hops.toString,
      "length_m" -> f"${geo.lengthMetres(route)}%.1f"
    )
    s"""{"type":"Feature","properties":${props(allProps)},"geometry":{"type":"LineString","coordinates":$coords}}"""

  /** A single node as a Point. */
  def point(p: LatLon, properties: Seq[(String, String)] = Seq.empty): String =
    s"""{"type":"Feature","properties":${props(properties)},"geometry":{"type":"Point","coordinates":${coord(p)}}}"""

  /** Every edge of the graph as a LineString.
    *
    * Only sensible for small graphs -- a city road network produces a file
    * hundreds of megabytes wide that no viewer will open. Use `sample` to thin
    * it, or export just the search frontier instead.
    */
  def edges(geo: GeoGraph, limit: Int = 20000): String =
    val features = Vector.newBuilder[String]
    var emitted = 0
    var u = 0
    while u < geo.numNodes && emitted < limit do
      var e = geo.graph.edgeBegin(u)
      val end = geo.graph.edgeEnd(u)
      while e < end && emitted < limit do
        val v = geo.graph.target(e)
        if u < v then // each undirected pair once
          val c = s"[${coord(geo.position(u))},${coord(geo.position(v))}]"
          features += s"""{"type":"Feature","properties":{"w":"${geo.graph.weight(e)}"},"geometry":{"type":"LineString","coordinates":$c}}"""
          emitted += 1
        e += 1
      u += 1
    collection(features.result())

  /** The set of nodes a search settled, as points.
    *
    * Rendering this for Dijkstra and A* side by side is the single most
    * convincing illustration of what a heuristic buys you: one is a disc
    * centred on the origin, the other a narrow cigar pointing at the target.
    */
  def settledNodes(geo: GeoGraph, tree: ShortestPathTree, limit: Int = 50000): String =
    val features = Vector.newBuilder[String]
    var emitted = 0
    var n = 0
    while n < geo.numNodes && emitted < limit do
      if !tree.dist(n).isInfinite then
        features += point(geo.position(n), Seq("dist" -> f"${tree.dist(n)}%.1f"))
        emitted += 1
      n += 1
    collection(features.result())

  def collection(features: Seq[String]): String =
    s"""{"type":"FeatureCollection","features":[${features.mkString(",")}]}"""

  def write(target: String, contents: String): FsPath =
    val path = Paths.get(target)
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
    path
