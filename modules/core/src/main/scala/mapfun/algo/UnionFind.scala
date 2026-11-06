package mapfun.algo

/** Disjoint-set forest with path compression and union by rank.
  *
  * The data structure behind Kruskal's MST ("would adding this edge close a
  * cycle?") and behind cheap connected-component counting. With both
  * optimisations, a sequence of m operations on n elements costs
  * O(m * alpha(n)) where alpha is the inverse Ackermann function -- under 5 for
  * any n you will ever build a graph from, so effectively constant.
  *
  * `find` is written iteratively in two passes rather than recursively: the
  * recursion depth before compression can reach n, and n here is a road
  * network.
  */
final class UnionFind(val size: Int):

  private val parent = Array.tabulate(size)(identity)
  private val rank = new Array[Byte](size)
  private var componentCount = size

  /** Number of disjoint sets remaining. */
  def components: Int = componentCount

  /** The representative of `x`'s set, compressing the path on the way. */
  def find(x: Int): Int =
    // Pass 1: walk to the root.
    var root = x
    while parent(root) != root do root = parent(root)
    // Pass 2: re-walk, pointing everything straight at the root.
    var cur = x
    while parent(cur) != root do
      val next = parent(cur)
      parent(cur) = root
      cur = next
    root

  def connected(a: Int, b: Int): Boolean = find(a) == find(b)

  /** Merges the sets containing `a` and `b`.
    *
    * Returns false if they were already the same set -- which is exactly the
    * "this edge would create a cycle" test Kruskal needs.
    */
  def union(a: Int, b: Int): Boolean =
    val ra = find(a)
    val rb = find(b)
    if ra == rb then false
    else
      // Hang the shallower tree off the deeper one so depth grows as slowly as
      // possible.
      val (small, large) =
        if rank(ra) < rank(rb) then (ra, rb) else (rb, ra)
      parent(small) = large
      if rank(ra) == rank(rb) then rank(large) = (rank(large) + 1).toByte
      componentCount -= 1
      true

  /** Component id per element, renumbered densely from 0. */
  def componentLabels: Array[Int] =
    val label = Array.fill(size)(-1)
    val result = new Array[Int](size)
    var next = 0
    var i = 0
    while i < size do
      val r = find(i)
      if label(r) == -1 then
        label(r) = next
        next += 1
      result(i) = label(r)
      i += 1
    result
