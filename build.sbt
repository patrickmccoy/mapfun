// mapfun -- a playground for map/graph algorithms and graph databases.
//
// Scala 3.3.x is the LTS line: every library below publishes for it, and the
// compiler happily reads TASTy produced by older 3.x releases. Bumping to a
// newer 3.x is safe in that direction; going backwards is not.

ThisBuild / scalaVersion := "3.3.8"
ThisBuild / organization := "fun.map"
ThisBuild / version      := "0.1.0-SNAPSHOT"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports",
  "-Ysafe-init"
)

// Bigger heaps than the sbt default: OSM extracts and all-pairs matrices are
// genuinely memory hungry, and running out of heap mid-import is a confusing
// way to learn about Floyd-Warshall.
ThisBuild / javaOptions ++= Seq("-Xmx4g", "-Xss8m")
ThisBuild / Test / fork  := true
ThisBuild / run / fork   := true
ThisBuild / run / connectInput := true


lazy val V = new {
  val catsEffect  = "3.6.3"
  val decline     = "2.6.2"
  val munit       = "1.3.4"
  val neotypes    = "1.2.2"
  // neotypes 1.2.x declares the Neo4j driver as `provided` and is built
  // against the 5.x line. The 6.x driver is a breaking change -- do not bump
  // this without also bumping neotypes.
  val neo4jDriver = "5.28.14"
  val osmpbf      = "1.6.1"
  val postgres    = "42.7.13"
}

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % V.munit % Test,
  // A forked run otherwise starts in the *module's* directory, so
  // `--pbf data/foo.osm.pbf` would resolve against modules/cli. Pin it to the
  // repository root so relative paths mean what they look like they mean.
  Compile / run / baseDirectory := (ThisBuild / baseDirectory).value
)

lazy val root = (project in file("."))
  .aggregate(core, osm, neo4j, age, cli)
  .settings(
    name           := "mapfun",
    publish / skip := true
  )

/** Graph representation and every algorithm. No I/O, no databases, no
  * dependencies beyond the standard library -- so it stays fast to compile and
  * easy to test.
  */
lazy val core = (project in file("modules/core"))
  .settings(commonSettings)
  .settings(name := "mapfun-core")

/** OpenStreetMap ingestion: .osm.pbf -> routable road graph. */
lazy val osm = (project in file("modules/osm"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := "mapfun-osm",
    libraryDependencies += "org.openstreetmap.pbf" % "osmpbf" % V.osmpbf
  )

/** Neo4j: the property-graph model, Cypher, and the Graph Data Science library. */
lazy val neo4j = (project in file("modules/graphdb-neo4j"))
  .dependsOn(core, osm)
  .settings(commonSettings)
  .settings(
    name := "mapfun-neo4j",
    libraryDependencies ++= Seq(
      "io.github.neotypes" %% "neotypes-core"        % V.neotypes,
      "io.github.neotypes" %% "neotypes-cats-effect" % V.neotypes,
      "io.github.neotypes" %% "neotypes-generic"     % V.neotypes,
      "org.neo4j.driver"    % "neo4j-java-driver"    % V.neo4jDriver,
      "org.typelevel"      %% "cats-effect"          % V.catsEffect
    )
  )

/** Apache AGE: openCypher grafted onto PostgreSQL. Deliberately uses plain
  * JDBC so the AGE-specific mechanics (LOAD, search_path, agtype) stay visible
  * rather than being hidden behind a query library.
  */
lazy val age = (project in file("modules/graphdb-age"))
  .dependsOn(core, osm)
  .settings(commonSettings)
  .settings(
    name := "mapfun-age",
    libraryDependencies += "org.postgresql" % "postgresql" % V.postgres
  )

/** Runnable entry point tying everything together. */
lazy val cli = (project in file("modules/cli"))
  .dependsOn(core, osm, neo4j, age)
  .settings(commonSettings)
  .settings(
    name := "mapfun-cli",
    libraryDependencies ++= Seq(
      "com.monovore"  %% "decline"        % V.decline,
      "com.monovore"  %% "decline-effect" % V.decline,
      "org.typelevel" %% "cats-effect"    % V.catsEffect
    )
  )
