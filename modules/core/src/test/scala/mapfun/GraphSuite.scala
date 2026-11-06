package mapfun

import mapfun.graph.*
import munit.FunSuite

class GraphSuite extends FunSuite:

  test("CSR packs edges into contiguous per-node runs") {
    val g = CsrGraph.directed(4)(
      (0, 1, 1.0),
      (0, 2, 2.0),
      (2, 3, 3.0)
    )
    assertEquals(g.numNodes, 4)
    assertEquals(g.numEdges, 3)
    assertEquals(g.outDegree(0), 2)
    assertEquals(g.outDegree(1), 0)
    assertEquals(g.outDegree(2), 1)
    assertEquals(g.neighbors(0).toSet, Set((1, 1.0), (2, 2.0)))
    assertEquals(g.neighbors(3), Vector.empty)
  }

  test("edge slots of a node are exactly its out-edges, and disjoint across nodes") {
    val g = CsrGraph.undirected(6)(
      (0, 1, 1.0),
      (1, 2, 1.0),
      (2, 3, 1.0),
      (3, 4, 1.0),
      (4, 5, 1.0),
      (5, 0, 1.0)
    )
    val seen = scala.collection.mutable.Set.empty[Int]
    var total = 0
    g.nodeIds.foreach { n =>
      (g.edgeBegin(n) until g.edgeEnd(n)).foreach { e =>
        assert(seen.add(e), s"edge slot $e claimed by two nodes")
        total += 1
      }
    }
    assertEquals(total, g.numEdges)
  }

  test("undirected construction adds both directions") {
    val g = CsrGraph.undirected(3)((0, 1, 5.0), (1, 2, 7.0))
    assertEquals(g.numEdges, 4)
    assertEquals(g.neighbors(1).toSet, Set((0, 5.0), (2, 7.0)))
  }

  test("reverse flips every edge and preserves the edge count") {
    val g = CsrGraph.directed(4)((0, 1, 1.0), (1, 2, 2.0), (0, 3, 4.0))
    val r = g.reverse
    assertEquals(r.numEdges, g.numEdges)
    assertEquals(r.neighbors(1).toSet, Set((0, 1.0)))
    assertEquals(r.neighbors(2).toSet, Set((1, 2.0)))
    assertEquals(r.neighbors(0), Vector.empty)
  }

  test("reverse is an involution") {
    val g = mapfun.gen.Generators.erdosRenyi(40, 0.1, seed = 7)
    val rr = g.reverse.reverse
    g.nodeIds.foreach(n => assertEquals(rr.neighbors(n).toSet, g.neighbors(n).toSet))
  }

  test("isolated nodes survive when reserved") {
    val b = GraphBuilder()
    b.addEdge(0, 1, 1.0)
    b.ensureNodes(10)
    val g = b.build()
    assertEquals(g.numNodes, 10)
    assertEquals(g.outDegree(9), 0)
  }

  test("subgraph renumbers survivors and drops edges to removed nodes") {
    val g = CsrGraph.directed(5)((0, 1, 1.0), (1, 2, 1.0), (2, 3, 1.0), (3, 4, 1.0))
    val (sub, remap) = g.subgraph(n => n != 2)
    assertEquals(sub.numNodes, 4)
    assertEquals(remap(2), NoNode)
    // 1 -> 2 and 2 -> 3 both vanish with node 2.
    assertEquals(sub.numEdges, 2)
    assertEquals(sub.neighbors(remap(0)).toSet, Set((remap(1), 1.0)))
    assertEquals(sub.neighbors(remap(1)), Vector.empty)
    assertEquals(sub.neighbors(remap(3)).toSet, Set((remap(4), 1.0)))
  }

  test("NodeCatalog assigns dense ids in first-seen order and round-trips") {
    val c = NodeCatalog.empty[Long]
    val a = c.intern(4207459283L)
    val b = c.intern(17L)
    val a2 = c.intern(4207459283L)
    assertEquals(a, 0)
    assertEquals(b, 1)
    assertEquals(a2, a)
    assertEquals(c.size, 2)
    assertEquals(c.labelOf(0), 4207459283L)
    assertEquals(c.idOf(99L), None)
  }
