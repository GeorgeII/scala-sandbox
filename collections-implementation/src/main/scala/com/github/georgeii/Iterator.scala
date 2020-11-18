package com.github.georgeii

trait Iterator[A] {

  def hasNext: Boolean

  def next(): A

}
