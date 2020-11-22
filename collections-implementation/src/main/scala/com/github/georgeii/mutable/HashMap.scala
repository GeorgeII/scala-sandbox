package com.github.georgeii.mutable

import com.github.georgeii

import scala.reflect.ClassTag

/**
 * A simple and primitive HashMap implementation. It includes 'Entry' data structure for holding keys and values.
 * This HashMap is implemented as a 'HashSet' of 'Entry'. Also, I do not use my previously built structures, i.e. HashSet
 * and ListBuffer, just in case there's an urgent need for more functionality.
 *
 * NOTE: there are generic data structures like HashTable and FlatHashTable in scala.collection.mutable package. They
 * are used in HashSet and HashMap internally. So, it's important to understand that my implementation via HashSet is
 * ad hoc to make it easier. But, at least, I tried to approximate as close as I could.
 * @tparam K key type.
 * @tparam V value type.
 */
class HashMap[K: ClassTag, V: ClassTag]
  extends georgeii.Map[K, V]{

  // No need to use hashtable here again because HashSet has its own one.
  private var setOfElements = scala.collection.mutable.HashSet[Entry[K, V]]()

  /**
   * A simple data structure for keeping a key and its value.
   * Overriding 'equals' and 'hashCode' is very important. We look for an element in the map by its key only.
   * For instance, a -> 10 and a -> 25 should point out to the same element in the HashSet as no duplicates are allowed.
   * @param key key of an element.
   * @param value null as a default value is very important here. It allows us to search for an element knowing only its key.
   * */
  private class Entry[K, V](val key: K, var value: V = null) {

    override def equals(other: Any): Boolean = other match {
      case that: Entry[K, V] => this.key == that.key
      case _ => false
    }

    override def hashCode(): Int = this.key.hashCode
  }

  def apply(key: K): V = {
    val element = setOfElements.find(elem => elem.key == key)

    if (element.isEmpty)
      throw new NoSuchElementException(s"key not found: $key")

    element.get.value
  }

  def update(key: K, value: V): Unit = {
    val element = setOfElements.find(elem => elem.key == key)

    if (element.isEmpty) {
      setOfElements += new Entry[K, V](key, value)
    } else {
      element.get.value = value
    }
  }

  def +=(tuple: (K, V)): Unit = update(tuple._1, tuple._2)

  def -=(key: K): Unit = {
    val element = setOfElements.find(elem => elem.key == key)

    if (element.isDefined)
      setOfElements -= element.get
  }

  def size: Int = setOfElements.size

  override def isEmpty: Boolean = size == 0

  override def iterator: georgeii.Iterator[(K, V)] = {
    new georgeii.Iterator[(K, V)] {
      private val iterOfSet = setOfElements.iterator


      override def hasNext: Boolean = iterOfSet.hasNext

      override def next(): (K, V) = {
        val nextElem = iterOfSet.next()
        (nextElem.key, nextElem.value)
      }
    }
  }

  override def foreach[U](f: ((K, V)) => U): Unit = {
    val iter = iterator

    while(iter.hasNext) f(iter.next())
  }
}
