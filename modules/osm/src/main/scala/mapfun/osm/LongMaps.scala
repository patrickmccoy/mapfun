package mapfun.osm

/** Fibonacci hashing shared by the maps below: multiply by 2^64/phi and keep
  * the top log2(capacity) bits. Cheap, and it scrambles the sequential-ish node
  * ids OSM hands out far better than a plain mask would.
  *
  * The shift has to track the capacity. A fixed shift keeps a fixed number of
  * bits, and once the table outgrows them every key lands in the bottom slots
  * and linear probing collapses.
  */
private[osm] object LongHash:
  inline def slot(key: Long, shift: Int): Int =
    ((key * 0x9e3779b97f4a7c15L) >>> shift).toInt

  def shiftFor(capacity: Int): Int =
    require(Integer.bitCount(capacity) == 1, s"capacity $capacity is not a power of two")
    64 - Integer.numberOfTrailingZeros(capacity)

/** Open-addressing hash map from `Long` keys to `Int` values.
  *
  * Written by hand rather than using `java.util.HashMap[java.lang.Long, Integer]`
  * because OSM ingestion is exactly the workload where boxing hurts most: a US
  * state extract has millions of node ids, and every one becomes a heap object
  * plus a `Node` wrapper in a chained hash map. Two flat arrays instead means
  * no allocation per entry, no pointer chasing, and roughly an order of
  * magnitude less memory.
  *
  * Linear probing with a power-of-two capacity, so the modulo is a mask. Key
  * `0L` is reserved as the empty marker -- legal here because OSM ids start at
  * 1. Deletion is not supported; nothing in the importer needs it.
  */
final class LongIntMap(initialCapacity: Int = 1 << 16):

  private var capacity = Integer.highestOneBit(math.max(16, initialCapacity) * 2)
  private var mask = capacity - 1
  private var shift = LongHash.shiftFor(capacity)
  private var keys = new Array[Long](capacity)
  private var values = new Array[Int](capacity)
  private var count = 0
  private var threshold = (capacity * 0.65).toInt

  def size: Int = count

  private inline def slotFor(key: Long): Int = LongHash.slot(key, shift)

  private def indexOf(key: Long): Int =
    var i = slotFor(key)
    while keys(i) != 0L && keys(i) != key do i = (i + 1) & mask
    i

  def contains(key: Long): Boolean =
    require(key != 0L, "key 0 is reserved")
    keys(indexOf(key)) == key

  def getOrElse(key: Long, default: Int): Int =
    val i = indexOf(key)
    if keys(i) == key then values(i) else default

  def put(key: Long, value: Int): Unit =
    require(key != 0L, "key 0 is reserved")
    val i = indexOf(key)
    if keys(i) == key then values(i) = value
    else
      keys(i) = key
      values(i) = value
      count += 1
      if count > threshold then grow()

  /** Adds `delta` to the value at `key`, treating a missing key as 0.
    *
    * This is the reference-counting primitive the junction detector is built
    * on, and doing it in one probe rather than a get-then-put matters when it
    * runs tens of millions of times.
    */
  def addTo(key: Long, delta: Int): Int =
    require(key != 0L, "key 0 is reserved")
    val i = indexOf(key)
    if keys(i) == key then
      values(i) += delta
      values(i)
    else
      keys(i) = key
      values(i) = delta
      count += 1
      if count > threshold then grow()
      delta

  private def grow(): Unit =
    val oldKeys = keys
    val oldValues = values
    capacity <<= 1
    mask = capacity - 1
    shift = LongHash.shiftFor(capacity)
    threshold = (capacity * 0.65).toInt
    keys = new Array[Long](capacity)
    values = new Array[Int](capacity)
    var i = 0
    while i < oldKeys.length do
      if oldKeys(i) != 0L then
        var j = slotFor(oldKeys(i))
        while keys(j) != 0L do j = (j + 1) & mask
        keys(j) = oldKeys(i)
        values(j) = oldValues(i)
      i += 1

  /** Visits every entry. Order is unspecified. */
  def foreach(f: (Long, Int) => Unit): Unit =
    var i = 0
    while i < keys.length do
      if keys(i) != 0L then f(keys(i), values(i))
      i += 1

/** As [[LongIntMap]], but storing a packed pair of `Int`s -- used to hold a
  * node's latitude and longitude as fixed-point 1e-7 degrees, which is exactly
  * the precision OSM itself stores and fits both into one `Long`.
  */
final class LongCoordMap(initialCapacity: Int = 1 << 16):

  private var capacity = Integer.highestOneBit(math.max(16, initialCapacity) * 2)
  private var mask = capacity - 1
  private var shift = LongHash.shiftFor(capacity)
  private var keys = new Array[Long](capacity)
  private var packed = new Array[Long](capacity)
  private var count = 0
  private var threshold = (capacity * 0.65).toInt

  def size: Int = count

  private inline def slotFor(key: Long): Int = LongHash.slot(key, shift)

  private def indexOf(key: Long): Int =
    var i = slotFor(key)
    while keys(i) != 0L && keys(i) != key do i = (i + 1) & mask
    i

  def put(key: Long, lat: Double, lon: Double): Unit =
    val i = indexOf(key)
    val value = LongCoordMap.pack(lat, lon)
    if keys(i) == key then packed(i) = value
    else
      keys(i) = key
      packed(i) = value
      count += 1
      if count > threshold then grow()

  def contains(key: Long): Boolean = keys(indexOf(key)) == key

  /** Returns the packed pair, or `Long.MinValue` when absent. */
  def getPacked(key: Long): Long =
    val i = indexOf(key)
    if keys(i) == key then packed(i) else Long.MinValue

  private def grow(): Unit =
    val oldKeys = keys
    val oldPacked = packed
    capacity <<= 1
    mask = capacity - 1
    shift = LongHash.shiftFor(capacity)
    threshold = (capacity * 0.65).toInt
    keys = new Array[Long](capacity)
    packed = new Array[Long](capacity)
    var i = 0
    while i < oldKeys.length do
      if oldKeys(i) != 0L then
        var j = slotFor(oldKeys(i))
        while keys(j) != 0L do j = (j + 1) & mask
        keys(j) = oldKeys(i)
        packed(j) = oldPacked(i)
      i += 1

object LongCoordMap:
  private val Scale = 1e7

  def pack(lat: Double, lon: Double): Long =
    val la = math.round(lat * Scale).toInt
    val lo = math.round(lon * Scale).toInt
    (la.toLong << 32) | (lo.toLong & 0xffffffffL)

  def unpackLat(p: Long): Double = (p >> 32).toInt / Scale
  def unpackLon(p: Long): Double = p.toInt / Scale
