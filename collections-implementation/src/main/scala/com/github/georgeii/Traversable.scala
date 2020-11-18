package com.github.georgeii

trait Traversable[A] {

  /**
   * The most important method of Traversable trait.
   *
   * @param f a function which is applied to every element in a collection.
   * @tparam U is almost always Unit type.
   */
  def foreach[U](f: A => U)

  def isEmpty: Boolean

}
