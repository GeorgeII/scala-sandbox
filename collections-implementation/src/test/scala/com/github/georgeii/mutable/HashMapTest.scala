package com.github.georgeii.mutable

import java.util.NoSuchElementException

import org.scalatest.flatspec.AnyFlatSpec

class HashMapTest extends AnyFlatSpec {
  "HashMap" should "be able to be declared as a structure of different types: whether it is Value or Reference type" in {
    val mapOfInts = new HashMap[Int, Int]()
    val mapOfStrings = new HashMap[String, String]()
  }

  it should "have size 0 after initialization" in {
    val map = new HashMap[String, String]()
    assert(map.size == 0)
  }

  it should "add elements" in {
    val map = new HashMap[String, String]
    map("a") = "aa"
    map("b") = "bb"
    map("c") = "cc"
    map("d") = "dd"
    assert(map.size == 4)

    for(i <- 5 to 1000) {
      map += (i.toString -> (i + "qwerty"))
    }
    assert(map.size == 1000)
  }

  it should "use 'foreach' method correctly" in {
    val map = new HashMap[Int, Int]
    val squares = scala.collection.mutable.HashMap[Int, Int]()
    map(1) = 1
    map(2) = 2
    map(3) = 3
    map(4) = 4

    map.foreach(entry => {
      squares(entry._1) = entry._2 * entry._2
    })

    assert(squares.size == 4)
    assert(squares(1) == 1)
    assert(squares(2) == 4)
    assert(squares(3) == 9)
    assert(squares(4) == 16)
  }

  it should "replace value if the key is already exist" in {
    val map = new HashMap[String, String]
    map("a") = "aa"
    map("b") = "bb"
    map("c") = "cc"
    map("d") = "dd"

    map("a") = "123"
    map("b") = "234"
    map("c") = "345"
    map += ("d" -> "456")

    assert(map("a") == "123")
    assert(map("b") == "234")
    assert(map("c") == "345")
    assert(map("d") == "456")
  }

  it should "remove elements from the map by its key" in {
    val map = new HashMap[String, String]
    map("a") = "aa"
    map("b") = "bb"
    map("c") = "cc"
    map("d") = "dd"

    map -= "a"
    map -= "c"

    assertThrows[NoSuchElementException] {
      map("a")
    }
    assertThrows[NoSuchElementException] {
      map("c")
    }
    assert(map.size == 2)
  }
}
