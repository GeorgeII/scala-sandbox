package com.github.georgeii

trait Iterable[A] extends Traversable[A] {

  def iterator: Iterator[A]

}
