package mapfun.graph

import scala.collection.mutable

/** Two-way mapping between external labels and the dense [[NodeId]] range the
  * algorithms need.
  *
  * OSM hands you node ids like `4207459283`; a `Long` that large cannot index
  * an array, and the ids in any given extract are sparse besides. Interning
  * them here once means the search loops never have to hash anything.
  *
  * Ids are handed out in first-seen order, so a catalog built twice from the
  * same input produces the same numbering -- which makes graph builds
  * reproducible and diffable.
  */
final class NodeCatalog[A] private (
    private val forward: mutable.HashMap[A, NodeId],
    private val backward: mutable.ArrayBuffer[A]
):

  def size: Int = backward.length

  /** Returns the id for `label`, assigning a fresh one if unseen. */
  def intern(label: A): NodeId =
    forward.get(label) match
      case Some(id) => id
      case None =>
        val id = backward.length
        forward.update(label, id)
        backward += label
        id

  def idOf(label: A): Option[NodeId] = forward.get(label)

  /** Unsafe lookup for hot paths where the label is known to be present. */
  def idOrFail(label: A): NodeId =
    forward.getOrElse(label, throw new NoSuchElementException(s"unknown label: $label"))

  def labelOf(id: NodeId): A = backward(id)

  def contains(label: A): Boolean = forward.contains(label)

  def labels: IndexedSeq[A] = backward.toIndexedSeq

  override def toString: String = s"NodeCatalog(${size} labels)"

object NodeCatalog:
  def empty[A]: NodeCatalog[A] =
    new NodeCatalog[A](mutable.HashMap.empty, mutable.ArrayBuffer.empty)

  def fromLabels[A](labels: IterableOnce[A]): NodeCatalog[A] =
    val c = empty[A]
    labels.iterator.foreach(c.intern)
    c
