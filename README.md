# mapfun

A playground for learning how maps actually work underneath: graph
representations, traversal, shortest paths, and the graph databases people store
road networks in. Written in Scala 3.

Nothing here is a dependency on a routing library. Every algorithm is
implemented from scratch, tested against the others, and commented with *why*
it is built the way it is — because the interesting part of routing is not the
pseudocode, it is the handful of decisions that make it work on real data.

```
sbt test                    # 91 tests, no setup required
sbt "cli/run demo"          # algorithm tour on synthetic graphs
./scripts/fetch-osm.sh monaco
sbt "cli/run route --pbf data/monaco-latest.osm.pbf --out out/monaco.geojson"
```

Drop the resulting `.geojson` on <https://geojson.io> to see the route on a real
map.

---

## Layout

| Module | What's in it |
|---|---|
| `modules/core` | Graph representation, all algorithms, geometry, generators. No dependencies beyond the standard library. |
| `modules/osm` | OpenStreetMap `.osm.pbf` → routable road graph. |
| `modules/graphdb-neo4j` | Neo4j via [neotypes](https://neotypes.github.io/neotypes/): Cypher, bulk loading, Graph Data Science. |
| `modules/graphdb-age` | Apache AGE (openCypher on PostgreSQL) via plain JDBC, plus the same traversals as recursive CTEs. |
| `modules/cli` | Runnable demos and benchmarks tying it together. |

---

## What's implemented

**Representation.** A compressed-sparse-row graph — three flat arrays, no
per-node objects. The `Graph` interface exposes *edge slots* rather than
neighbour collections, which means iteration allocates nothing and a
half-finished traversal is just an `Int`. That second property is what makes the
iterative DFS and Tarjan possible, and you need those: a recursive DFS overflows
the stack on a real road network, and there is a test that proves it.

**Traversal.** BFS, iterative DFS, topological sort (Kahn), cycle detection,
two-colouring, weakly connected components, Tarjan's strongly connected
components.

**Shortest paths.** Dijkstra with an indexed binary heap supporting
decrease-key, A\* with pluggable heuristics, bidirectional Dijkstra,
Bellman-Ford with negative-cycle detection, Floyd-Warshall, 0-1 BFS, and Yen's
k-shortest-loopless-paths.

**Other.** Kruskal and Prim MSTs, union-find, haversine and Mercator geometry, a
grid spatial index for snapping coordinates to junctions, graph generators
(grid, maze, random geometric, Erdős–Rényi, DAG), and GeoJSON export.

### Things the tests pin down

The suite mostly checks algorithms *against each other* rather than against
hand-written answers, which catches far more:

- BFS ≡ Dijkstra on unit weights; 0-1 BFS ≡ Dijkstra on 0/1 weights.
- Floyd-Warshall ≡ Dijkstra from every source.
- A\* ≡ Dijkstra across many random queries, on grids and on mazes.
- A\* with the zero heuristic ≡ Dijkstra exactly (Dijkstra *is* the special case).
- Tarjan's components ≡ mutual reachability, checked by brute force.
- Kruskal ≡ Prim on total weight.
- There is a test asserting Dijkstra gets the **wrong** answer on a negative
  edge, so the precondition is documented by demonstration rather than by
  comment.

---

## The learning path

Rough order of increasing depth. Each step has something concrete to run.

### 1. Get the shape of a graph in your head

```
sbt "cli/run demo"
```

Read `modules/core/src/main/scala/mapfun/graph/Graph.scala` alongside it. The
question worth sitting with: why does the interface hand out edge *slots*
instead of a list of neighbours?

### 2. Understand why A\* sometimes does nothing

The demo prints this, and it is the most useful thing in the project:

```
  settled       10000       9901        199       9993
             Dijkstra     A*(L2)    A*(L1)   bidirectional
```

All four return the identical route. On a 4-connected grid the true remaining
distance is the taxicab (L1) distance; straight-line (L2) distance is up to √2
smaller. Both are admissible, so both are *correct* — but the weak one prunes
almost nothing. **Admissibility buys the right answer; tightness buys speed.**

Then look at the maze case, where straight-line distance points confidently
through walls and A\* degenerates to Dijkstra entirely.

### 3. Route on real data

```
./scripts/fetch-osm.sh monaco        # ~700 KB, good first run
./scripts/fetch-osm.sh rhode-island  # ~30 MB, a real network
sbt "cli/run inspect --pbf data/rhode-island-latest.osm.pbf"
sbt "cli/run route   --pbf data/rhode-island-latest.osm.pbf --out out/route.geojson"
```

`modules/osm/src/main/scala/mapfun/osm/OsmImporter.scala` is where the real-data
lessons live. Three ideas do most of the work:

- **Two passes.** Nodes come before ways in the file, but you cannot tell which
  nodes matter until you have seen the ways.
- **Junction contraction.** An OSM way has a node at every bend in the road.
  Those are geometry, not decisions — there is nowhere to turn. Collapsing them
  shrinks the graph 5–10×.
- **Largest strongly connected component.** A clipped extract is full of roads
  whose continuation was cut off at the boundary. Without this step, routing
  fails for reasons that have nothing to do with your query.

Things to try: compare `--cost time` against `--cost distance` and see where the
routes diverge (the fast one takes the motorway). Compare `--profile car`
against `--profile foot` — pedestrians ignore one-way streets, so the walking
graph is far better connected.

### 4. Put it in a graph database

```
docker compose up -d          # Neo4j on 7687, Postgres/AGE on 5433
sbt "cli/run load-neo4j --pbf data/monaco-latest.osm.pbf"
sbt "cli/run neo4j --from 0 --to 500 --gds --pbf data/monaco-latest.osm.pbf"
sbt "cli/run load-age --pbf data/monaco-latest.osm.pbf --limit 400"
sbt "cli/run age --from 0 --hops 3"
```

Neo4j browser at <http://localhost:7474> (`neo4j` / `mapfunmapfun`). Neo4j takes
30–60 seconds to become healthy on first boot while it downloads the GDS plugin.

Passing `--pbf` to the `neo4j` subcommand re-imports the extract in memory and
compares `core`'s Dijkstra against GDS's:

```
-- core vs GDS --
  core Dijkstra: 191.103  (settled 1072)
  gds  Dijkstra: 191.103
  they agree: true
```

That is the strongest check in the project — it exercises the importer, the
graph build, the search, the Bolt load and the GDS projection at once, and two
independent implementations have to land on the same number.

One trap worth knowing about, because it is easy to hit and the error message is
unhelpful: a GDS *projection* carries only the properties you name. Omit
`sourceNodeProperties`/`targetNodeProperties` and Dijkstra works fine while A\*
fails with `The property 'lat' has not been loaded` — the algorithms see the
projection, not the stored nodes.

Be clear-eyed about what this buys you. **It is not speed** — the in-memory
Dijkstra in `core` beats a round trip to any database, every time. What you get
is a *queryable* graph: pattern matching over structure, ad-hoc traversals you
did not write an algorithm for, and persistence.

### 5. Compare the two database models

The two modules are written in deliberately different styles, because the
comparison is the lesson:

|  | Neo4j | Apache AGE |
|---|---|---|
| Query language | Cypher | openCypher (a subset) |
| Scala access | neotypes — typed, `ResultMapper`, cats-effect `IO` | plain JDBC, parse `agtype` yourself |
| Algorithms | GDS library: Dijkstra, A\*, Yen, PageRank, betweenness | none — write them yourself |
| Bulk load | fast, `UNWIND` batches | slow, ordinary Postgres inserts |
| Killer feature | mature tooling and algorithms | joins to your existing relational tables |

`modules/graphdb-age` also implements the same traversals as **recursive CTEs**
against a plain edge table, with no graph extension at all. Run `cli/run age`
and compare. A `WITH RECURSIVE` query *is* a graph traversal — it walks the edge
table exactly the way BFS walks an adjacency list. Anyone claiming you need a
graph database to traverse a graph should be asked to explain it.

Note what the CTE version cannot do: it has no priority queue, so
`RecursiveCte.shortestCosts` is Bellman-Ford, not Dijkstra. It explores every
path under a hop limit and takes the minimum. Seeing exactly *why* SQL forces
that is worth more than the timing number.

### 6. Benchmark honestly

```
sbt "cli/run bench --pbf data/rhode-island-latest.osm.pbf --queries 200"
```

Warms up the JIT before timing anything, because skipping that can make the
first algorithm measured look 10× slower than it is.

---

## Where to go next

The natural continuations, roughly in order of payoff:

1. **Contraction Hierarchies.** *The* technique behind continent-scale routing.
   Preprocess by repeatedly contracting the least-important node and inserting
   shortcut edges; queries then run bidirectionally, only ever moving "upward"
   in importance. Millisecond routes across a continent. This is the single
   biggest thing missing here.
2. **ALT (A\*, Landmarks, Triangle inequality).** Pick a handful of landmarks,
   precompute distances to them, and use the triangle inequality for a heuristic
   far tighter than straight-line distance. Directly addresses the weak-heuristic
   problem step 2 makes visible.
3. **Turn restrictions and turn costs.** Real routing forbids specific turns and
   penalises others. The standard trick is an *edge-based* graph, where nodes
   become road segments and edges become turns — a genuinely different model.
4. **Time-dependent weights.** Edge cost as a function of departure time.
   Dijkstra still works if the network is FIFO; it breaks if not.
5. **Map matching.** Snap a noisy GPS trace to the road network with a hidden
   Markov model.
6. **Isochrones.** Everything reachable within 20 minutes — a one-to-many
   Dijkstra plus a concave hull.

---

## Notes on the build

- Scala 3.3.8 (LTS). Newer 3.x compilers read older TASTy fine, so bumping
  forward is safe; going backwards is not.
- neotypes 1.2.2 declares the Neo4j driver as `provided` and targets the **5.x**
  line. The 6.x driver is a breaking change — do not bump one without the other.
- `Test / fork` and `run / fork` are on with a 4 GB heap and an 8 MB stack. OSM
  extracts and all-pairs matrices are genuinely memory hungry.
- `osm4scala` has no Scala 3 build, so the importer uses the official `osmpbf`
  Java bindings and decodes blocks directly. That turns out to be the more
  instructive path anyway — the delta encoding of dense nodes and way refs is
  visible rather than hidden.
- Apache AGE image tags encode both the Postgres major and the AGE release
  (`release_PG16_1.6.0`); there is no rolling `PG16_latest`. Check
  <https://hub.docker.com/r/apache/age/tags> before bumping.

Everything above has been run: 87 tests pass, the Monaco extract routes
end-to-end, and both databases have been loaded and queried against live
containers.

## Known limitations

- OSM access-tag handling is deliberately shallow. Real access rules involve
  conditional restrictions and a vehicle-type hierarchy; this handles the common
  cases that produce visibly wrong routes and no more.
- `maxspeed` parsing handles numbers and `mph`, not the country-code forms
  (`RU:urban`). Those fall back to the road-class default table in
  `RoadClass.scala`, which is a set of ordinary assumptions, not measurements —
  change them and watch the routes change.
- No turn restrictions, so routes may include U-turns that are illegal in
  practice.
- `Agtype` parsing covers the shapes this project returns, not the format in
  general.
- The database `load` commands cap the graph size by default (`--limit`), since
  pushing a million junctions over Bolt is not the lesson.

## History

mapfun was developed locally with Claude Code as a pair-programmer and imported
here as a single commit. The algorithm choices, data structures and tests are
mine to explain. I built it to learn this material properly, and the comments
document why each piece works the way it does.

## License

MIT — see [LICENSE](LICENSE).
