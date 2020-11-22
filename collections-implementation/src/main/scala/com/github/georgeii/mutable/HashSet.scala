package com.github.georgeii.mutable

import com.github.georgeii

import scala.reflect.ClassTag
import scala.util.control.Breaks._

/**
 * A simple HashSet implementation using hashtable.
 * Hashtable is an array of Lists. Every List represents a so-called bucket.
 * It needs to be done in order to cope with hash collisions.
 * Worst time-complexity of HashSet is O(n) (it happens only due to a poor-built hash function when every element gives
 * the same hash).
 * @tparam A - type of elements.
 */
class HashSet[A: ClassTag]
  extends georgeii.Set[A] {

  // it's a little bit better to have hashtable size multiple of 2 (as we divide hashcodes by hashtable length).
  private var hashtable = Array.ofDim[scala.collection.mutable.ListBuffer[A]](32)

  private var numberOfElements = 0

  /** Adds a new element to the set. */
  def +=(element: A): Unit = {
    // enlarge hashtable when there are 2x elements, where x is a length of a hashtable.
    if (numberOfElements >= 2 * hashtable.length) {
      expandHashTable()
    }

    val index = element.hashCode % hashtable.length

    // if the bucket has not been initialized yet.
    if (hashtable(index) == null) {
      hashtable(index) = new scala.collection.mutable.ListBuffer[A]()
    }

    val bucket = hashtable(index)

    // do nothing if the element is already in a set.
    if (bucket.contains(element))
      return

    bucket += element
    numberOfElements += 1
  }

  /**
   * Increases the number of buckets (2 times larger) in order to save the Set asymptotic constant-time efficiency.
   * Recalculates the whole hashtable so its elements can be moved to another bucket number in the new hashtable.
   */
  private def expandHashTable(): Unit = {
    // !!! TODO: check if it's possible without ClassTag !!!
    val newHashTable = new HashSet[A]()
    newHashTable.initialSize(2 * hashtable.length)

    // put every element to the new hashtable.
    this.foreach(x => newHashTable += x)

    // make a deep copy of the new hash table.
    hashtable = newHashTable.getDeepCopyOfHashTable
  }

  private def initialSize(n: Int): Unit = {
    if (numberOfElements == 0)
      hashtable = Array.ofDim[scala.collection.mutable.ListBuffer[A]](n)
    else throw new RuntimeException("There was an illegal attempt to change the hashtable. Method initialSize is" +
      " allowed only if a set does not contain elements.")
  }

  private def getDeepCopyOfHashTable: Array[scala.collection.mutable.ListBuffer[A]] = {
    hashtable.clone.map(_.clone)
  }

  def contains(element: A): Boolean = {
    val index = element.hashCode % hashtable.length
    val bucket = hashtable(index)

    if (bucket == null)
      return false

    bucket.contains(element)
  }

  def size: Int = numberOfElements

  override def isEmpty: Boolean = size == 0

  override def iterator: georgeii.Iterator[A] = {
    new georgeii.Iterator[A] {
      private var iteratorIndex = 0
      private var hashtableIndex = 0
      private var elementsPassedInConcreteBucket = 0

      override def hasNext: Boolean = iteratorIndex < numberOfElements

      override def next(): A = {
        var bucket = new scala.collection.mutable.ListBuffer[A]

        breakable {
          for (i <- hashtableIndex until hashtable.length) {
            bucket = hashtable(hashtableIndex)

            if (bucket != null && bucket.nonEmpty && elementsPassedInConcreteBucket < bucket.length) {
              break
            }

            hashtableIndex = i
            elementsPassedInConcreteBucket = 0
          }
        }

        val element = bucket(elementsPassedInConcreteBucket)
        elementsPassedInConcreteBucket += 1

        element
      }
    }
  }

  override def foreach[U](f: A => U): Unit = {
    val iter = iterator

    while(iter.hasNext) f(iter.next())
  }
}
