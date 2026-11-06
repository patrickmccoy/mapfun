package mapfun

import mapfun.algo.*
import munit.FunSuite
import scala.util.Random

class DataStructureSuite extends FunSuite:

  test("heap pops in ascending key order") {
    val h = IndexedMinHeap(10)
    Seq(5 -> 5.0, 1 -> 1.0, 3 -> 3.0, 9 -> 9.0, 0 -> 0.5).foreach((e, k) => h.push(e, k))
    val popped = Iterator.continually(h.popMin()).takeWhile(_ => true).take(5).toList
    assertEquals(popped, List(0, 1, 3, 5, 9))
    assert(h.isEmpty)
  }

  test("push on a queued element lowers its key and never raises it") {
    val h = IndexedMinHeap(10)
    h.push(1, 10.0)
    h.push(2, 5.0)
    h.push(1, 1.0) // now the smallest
    assertEquals(h.popMin(), 1)
    h.push(2, 100.0) // ignored: worse than the existing key
    assertEquals(h.keyOf(2), 5.0)
    assertEquals(h.popMin(), 2)
  }

  test("heap survives randomised push/pop against a sorted reference") {
    val rng = Random(1234)
    val n = 500
    val h = IndexedMinHeap(n)
    val keys = scala.collection.mutable.HashMap.empty[Int, Double]

    for _ <- 0 until 5000 do
      val e = rng.nextInt(n)
      val k = rng.nextDouble() * 1000
      val previous = keys.get(e)
      h.push(e, k)
      // Mirror the heap's semantics: insert, or lower an existing key.
      keys.update(e, previous.fold(k)(p => math.min(p, k)))
      assert(h.isValid, "heap invariant broken after push")

    val drained = Iterator.continually(h.popMin()).take(h.size).toList
    val expected = keys.toList.sortBy(_._2).map(_._1)
    assertEquals(drained.map(keys), expected.map(keys))
  }

  test("union-find merges sets and counts components") {
    val uf = UnionFind(6)
    assertEquals(uf.components, 6)
    assert(uf.union(0, 1))
    assert(uf.union(1, 2))
    assert(!uf.union(0, 2)) // already connected -- this is Kruskal's cycle test
    assertEquals(uf.components, 4)
    assert(uf.connected(0, 2))
    assert(!uf.connected(0, 3))
  }

  test("union-find labels are dense and agree with connectivity") {
    val uf = UnionFind(8)
    uf.union(0, 1); uf.union(2, 3); uf.union(3, 4)
    val labels = uf.componentLabels
    assertEquals(labels.distinct.length, uf.components)
    assertEquals(labels.max, uf.components - 1)
    assertEquals(labels(0), labels(1))
    assertEquals(labels(2), labels(4))
    assertNotEquals(labels(0), labels(2))
  }

  test("union-find handles a long chain without stack overflow") {
    val n = 200000
    val uf = UnionFind(n)
    (0 until n - 1).foreach(i => uf.union(i, i + 1))
    assertEquals(uf.components, 1)
    assert(uf.connected(0, n - 1))
  }
