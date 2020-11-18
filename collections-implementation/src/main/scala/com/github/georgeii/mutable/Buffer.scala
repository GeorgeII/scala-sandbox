package com.github.georgeii.mutable

/**
 * The most popular implementation of Buffer is ArrayBuffer.
 */
trait Buffer[A] extends Seq[A]{

  /** Adds a new element in the end of a Buffer. */
  def append(element: A)

  /** Adds a new element as a first one. */
  def prepend(element:A)

  def insert(idx: Int, element:A)

  def remove(idx: Int)

}
