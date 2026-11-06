package mapfun.algo

/** A binary min-heap that also knows where each element currently sits, so a
  * key can be lowered in place.
  *
  * Dijkstra needs `decrease-key`: when a shorter route to an already-queued
  * node turns up, that node has to move up the queue. `java.util.PriorityQueue`
  * cannot do this, so the usual workaround is "lazy deletion" -- push a second
  * entry for the node and ignore the stale one when it pops. That works, and is
  * what most tutorial implementations do, but the queue then holds up to one
  * entry per *edge* rather than per *node*.
  *
  * This version keeps a `positions` array mapping element -> heap slot, which
  * caps the queue at one entry per node and makes `decreaseKey` O(log n). On a
  * road network -- where the average node has degree ~2.5 and most relaxations
  * do improve a key -- it is the difference between a queue of millions and a
  * queue of thousands.
  *
  * Elements are `Int`s in `[0, capacity)`, which is exactly what [[mapfun.graph.NodeId]]
  * already is. Not thread-safe, and deliberately mutable: this is the inner
  * loop.
  */
final class IndexedMinHeap(capacity: Int):

  require(capacity >= 0, "capacity must be non-negative")

  /** heap slot -> element */
  private val heap = new Array[Int](capacity)

  /** element -> heap slot, or -1 when the element is not queued */
  private val positions = Array.fill(capacity)(-1)

  /** element -> its key */
  private val keys = new Array[Double](capacity)

  private var count = 0

  def size: Int = count
  def isEmpty: Boolean = count == 0
  def nonEmpty: Boolean = count > 0

  def contains(element: Int): Boolean = positions(element) >= 0

  /** The key of a queued element. Meaningless for elements never inserted. */
  def keyOf(element: Int): Double = keys(element)

  def clear(): Unit =
    var i = 0
    while i < count do
      positions(heap(i)) = -1
      i += 1
    count = 0

  /** Inserts `element`, or lowers its key if already queued and the new key is
    * better. The one call site Dijkstra needs.
    */
  def push(element: Int, key: Double): Unit =
    val p = positions(element)
    if p < 0 then
      keys(element) = key
      heap(count) = element
      positions(element) = count
      count += 1
      siftUp(count - 1)
    else if key < keys(element) then
      keys(element) = key
      siftUp(p)

  /** Removes and returns the element with the smallest key. */
  def popMin(): Int =
    require(count > 0, "popMin on an empty heap")
    val min = heap(0)
    count -= 1
    positions(min) = -1
    if count > 0 then
      val moved = heap(count)
      heap(0) = moved
      positions(moved) = 0
      siftDown(0)
    min

  def peekKey: Double =
    require(count > 0, "peekKey on an empty heap")
    keys(heap(0))

  private def siftUp(start: Int): Unit =
    var i = start
    val element = heap(i)
    val key = keys(element)
    var continue = true
    while i > 0 && continue do
      val parent = (i - 1) >>> 1
      val parentElement = heap(parent)
      if keys(parentElement) <= key then continue = false
      else
        heap(i) = parentElement
        positions(parentElement) = i
        i = parent
    heap(i) = element
    positions(element) = i

  private def siftDown(start: Int): Unit =
    var i = start
    val element = heap(i)
    val key = keys(element)
    var continue = true
    while continue do
      val left = 2 * i + 1
      if left >= count then continue = false
      else
        val right = left + 1
        val child =
          if right < count && keys(heap(right)) < keys(heap(left)) then right else left
        val childElement = heap(child)
        if keys(childElement) >= key then continue = false
        else
          heap(i) = childElement
          positions(childElement) = i
          i = child
    heap(i) = element
    positions(element) = i

  /** Checks the heap property over the whole structure. Tests only -- O(n). */
  private[mapfun] def isValid: Boolean =
    var ok = true
    var i = 1
    while i < count do
      if keys(heap((i - 1) >>> 1)) > keys(heap(i)) then ok = false
      if positions(heap(i)) != i then ok = false
      i += 1
    if count > 0 && positions(heap(0)) != 0 then ok = false
    ok
