package com.github.georgeii.mutable

/**
 * ArrayBuffer is a typical resizable array. It can be considered the closest collection to ArrayList in Java.
 * @param elems array of initial elements can be passed into constructor.
 */
class ArrayBuffer[A](private var elems: Array[A] = null) extends Buffer[A] with IndexedSeq[A] {
  var elements: Array[A] = null
  var numberOfElements: Int = 0

  // if nothing was passed in the constructor - create an empty array of length 5
  if (elems == null) {
    this.elements = new Array[A](5)
  }
  else {
    elements = elems.clone()
  }

  /** Adds a new element in the end of a Buffer. */
  override def append(element: A): Unit = insert(numberOfElements - 1, element)

  /** Adds a new element as a first one. */
  override def prepend(element: A): Unit = insert(0, element)

  /** Inserts an element in the current array. */
  override def insert(idx: Int, element: A): Unit = {
    if (idx < 0 || idx >= numberOfElements) {
      throw new ArrayIndexOutOfBoundsException
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
      throw new ArrayIndexOutOfBoundsException
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

  override def apply(i: Int): A = ???

  override def length: Int = numberOfElements

}
