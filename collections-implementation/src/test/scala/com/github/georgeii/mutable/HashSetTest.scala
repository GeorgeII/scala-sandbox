package com.github.georgeii.mutable

import org.scalatest.flatspec.AnyFlatSpec

class HashSetTest extends AnyFlatSpec {
  "A set" should "be able to be declared as a structure of different types: whether it is Value or Reference type" in {
    val setOfInts = new HashSet[Int]()
    val setOfStrings = new HashSet[String]()
  }

  it should "have size 0 after initialization" in {
    val set = new HashSet[Int]()
    assert(set.size == 0)
  }

  it should "add elements and implement resizable hashtable, i.e. rebuild a hashtable for a larger one when there's " +
    "too many elements" in {
    val set = new HashSet[Int]()
    set += 1
    set += 2
    set += 3
    set += 4
    assert(set.size == 4)

    for(i <- 5 to 1000) {
      set += i
    }
    assert(set.size == 1000)
  }

  /**
   * This test works only in special cases because Set does not guarantee the order of elements so it's hard to assert
   * result of 'foreach' method.
   */
  it should "use 'foreach' method correctly" in {
    val set = new HashSet[Int]
    val squares = scala.collection.mutable.ArrayBuffer[Int]()
    set += 1
    set += 2
    set += 3
    set += 4

    set.foreach(x => squares.append(x * x))

    assert(squares(0) == 1)
    assert(squares(0) == 4)
    assert(squares(0) == 9)
    assert(squares(0) == 16)
  }

  it should "not add an element which is already in a collection" in {
    val set = new HashSet[String]()
    set += "One"
    set += "Two"
    set += "Three"
    set += "Three"
    set += "Three"
    set += "Two"
    assert(set.size == 3)
  }
}
