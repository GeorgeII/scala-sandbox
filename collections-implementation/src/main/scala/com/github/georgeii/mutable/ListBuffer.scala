package com.github.georgeii.mutable

import com.github.georgeii

import scala.reflect.ClassTag

/**
 * A typical linked list is implemented under the hood. Can be initialized from the input Array.
 * @tparam A type of elements.
 */
class ListBuffer[A: ClassTag]
  extends georgeii.mutable.Buffer[A] {

  private var head: Option[Node[A]] = None
  private var tail: Option[Node[A]] = None
  private var numberOfNodes = 0


  private class Node[A: ClassTag](var value: Option[A], var next: Option[Node[A]] = None)

  /** Adds a new element in the end of a Buffer. */
  override def append(element: A): Unit = {
    // if the list is non-empty
    if (tail.isDefined) {
      val newTail = new Node[A](Option(element))
      tail.get.next = Option(newTail)
      tail = Option(newTail)
    }
    else {
      initializeList(element)
    }

    numberOfNodes += 1
  }

  /** Adds a new element as a first one. */
  override def prepend(element: A): Unit = {
    // if the list is non-empty
    if (head.isDefined) {
      val newHead = new Node[A](Option(element), head)
      head = Option(newHead)
    }
    else {
      initializeList(element)
    }

    numberOfNodes += 1
  }

  /** Adds the very 1st node */
  private def initializeList(element: A): Unit = {
    val node = new Node[A](Option(element))
    tail = Option(node)
    head = Option(node)
  }

  override def insert(idx: Int, element: A): Unit = {
    if (idx < 0 || idx >= numberOfNodes) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfNodes elements.")
    }

    if (idx == 0) {
      val newNode = new Node[A](Option(element), head)
      head = Option(newNode)
      return
    }

    var currentNode = head.get
    for(_ <- 0 until idx - 1) {
      currentNode = currentNode.next.get
    }

    val disconnectedNode = currentNode.next.get
    val newNode = new Node[A](Option(element), Option(disconnectedNode))
    currentNode.next = Option(newNode)

    if (idx == 0) {
      head = Option(currentNode)
    }

    numberOfNodes += 1
  }

  override def remove(idx: Int): Unit = {
    if (idx < 0 || idx >= numberOfNodes) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfNodes elements.")
    }

    // Handle a couple of corner cases below.
    if (idx == 0 && numberOfNodes == 1) {
      tail = None
      head = None
      numberOfNodes -= 1
      return
    }

    // if we delete the 1st element and a list contains more than this.
    if (idx == 0) {
      val newHead = head.get.next
      head.get.next = None
      head = newHead
      numberOfNodes -= 1
      return
    }

    var currentNode = head.get
    for(_ <- 0 until idx - 1) {
      currentNode = currentNode.next.get
    }

    val nodeAfterDeleteNode = currentNode.next.get.next

    nodeAfterDeleteNode match {
      case Some(node) => currentNode.next = Some(node)
      case None       =>
        currentNode.next = None
        tail = Option(currentNode)
    }

    numberOfNodes -= 1
  }

  def apply(idx: Int): A = {
    if (idx < 0 || idx >= numberOfNodes) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfNodes elements.")
    }

    var currentNode = head.get
    for(_ <- 0 until idx) {
      currentNode = currentNode.next.get
    }

    currentNode.value.get
  }

  def update(idx: Int, element: A) {
    if (idx < 0 || idx >= numberOfNodes) {
      throw new ArrayIndexOutOfBoundsException(s"$idx was entered as an index. " +
        s"The ArrayBuffer contains only $numberOfNodes elements.")
    }

    var currentNode = head.get
    for (_ <- 0 until idx) {
      currentNode = currentNode.next.get
    }

    currentNode.value = Option(element)
  }

  override def length: Int = numberOfNodes

  override def iterator: georgeii.Iterator[A] = {
    new georgeii.Iterator[A] {
      private var currentNode: Node[A] = new Node[A](None, head)

      override def hasNext: Boolean = currentNode.next.isDefined

      override def next(): A = {
        currentNode = currentNode.next.get
        currentNode.value.get
      }
    }
  }

  /** Implement foreach from Traversible */
  override def foreach[U](f: A => U): Unit = {
    val iter = iterator

    while(iter.hasNext) f(iter.next())
  }
}
