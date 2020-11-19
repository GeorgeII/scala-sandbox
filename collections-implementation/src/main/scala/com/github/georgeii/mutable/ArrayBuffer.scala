package com.github.georgeii.mutable

import scala.reflect.ClassTag
import com.github.georgeii

/**
 * ArrayBuffer is a typical resizable array. It can be considered the closest collection to ArrayList in Java.
 *
 * @param elems array of initial elements can be passed into constructor.
 */
class ArrayBuffer[A: ClassTag](private var elems: Array[A] = null)
  extends georgeii.mutable.Buffer[A]
    with georgeii.IndexedSeq[A] {

  // I allowed myself to use null here because it's a totally hidden implementation.
  var elements: Array[A] = null
  var numberOfElements: Int = 0

  // if nothing was passed in the constructor - create an empty array of length 5
  if (elems == null) {
    elements = new Array[A](5)
  }
  else {
    elements = elems.clone()
  }

  /** Adds a new element in the end of a Buffer. */
  override def append(element: A): Unit = insert(numberOfElements, element)

  /** Adds a new element as a first one. */
  override def prepend(element: A): Unit = insert(0, element)

  /** Inserts an element in the current array. */
  override def insert(idx: Int, element: A): Unit = {
    if (idx < 0 || idx > numberOfElements) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfElements elements.")
    }

    // if the internal array is full - expand it by x2.
    if (elements.length == numberOfElements) {
      elements = Array.copyOf(elements, numberOfElements * 2)
    }

    val tempArray = new Array[A](numberOfElements - idx)
    Array.copy(elements, idx, tempArray, 0, tempArray.length)
    elements(idx) = element
    Array.copy(tempArray, 0, elements, idx + 1, tempArray.length)

    numberOfElements += 1
  }

  /** Removes an element from the array by its index. */
  override def remove(idx: Int): Unit = {
    if (idx < 0 || idx >= numberOfElements) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfElements elements.")
    }

    // if elements take only 1/4 of the whole array - shrink it by x2.
    if (elements.length / 4 >= numberOfElements && elements.length > 20) {
      elements = Array.copyOf(elements, numberOfElements / 2)
    }

    val tempArray = new Array[A](numberOfElements - idx)
    Array.copy(elements, idx, tempArray, 0, tempArray.length)
    Array.copy(tempArray, 0, elements, idx, tempArray.length)

    numberOfElements -= 1
  }

  def apply(idx: Int): A = {
    if (idx < 0 || idx >= numberOfElements) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfElements elements.")
    }

    elements(idx)
  }

  def update(idx: Int, element: A) {
    if (idx < 0 || idx >= numberOfElements) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfElements elements.")
    }

    elements(idx) = element
  }

  override def length: Int = numberOfElements

  /** Implement method from Iterable */
  override def iterator: georgeii.Iterator[A] = {
    new georgeii.Iterator[A] {

      private[this] var iteratorIndex = -1

      override def hasNext: Boolean = {iteratorIndex + 1 > numberOfElements}

      override def next(): A = {
        iteratorIndex += 1
        elements(iteratorIndex)
      }
    }
  }

  /** Implement foreach from Traversible */
  override def foreach[U](f: A => U): Unit = {
    val iter = iterator

    while(iter.hasNext) f(iter.next())
  }
}
