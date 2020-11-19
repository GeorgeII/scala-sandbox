package com.github.georgeii

import com.github.georgeii

/**
 * Seq must always have a strict order of elements. That means it will always be iterated in a certain order.
 * @tparam A type of its elements.
 */
trait Seq[A] extends georgeii.Iterable[A]{

  def length: Int

  override def isEmpty: Boolean = length == 0

}
