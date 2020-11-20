package com.github.georgeii.mutable

import org.scalatest.flatspec.AnyFlatSpec

class ListBufferTest extends AnyFlatSpec {

  "ListBuffer" should "have length 0 after initialization" in {
    val buffer = new ListBuffer[Int]()
    assert(buffer.length == 0)
  }

  it should "be able to be declared as a structure of different types: whether it is Value or Reference type" in {
    val bufferOfInts = new ListBuffer[Int]()
    bufferOfInts.append(1)
    bufferOfInts.append(2)
    bufferOfInts.append(3)

    assert(bufferOfInts(0) == 1)
    assert(bufferOfInts(1) == 2)
    assert(bufferOfInts(2) == 3)

    val bufferOfOptions = new ListBuffer[Option[Int]]()
    bufferOfOptions.append(Option(1))
    bufferOfOptions.append(Option(2))
    bufferOfOptions.append(Option(3))

    assert(bufferOfOptions(0).get == 1)
    assert(bufferOfOptions(1).get == 2)
    assert(bufferOfOptions(2).get == 3)
  }

  it should "insert elements correctly in the middle of the buffer" in {
    val buffer = new ListBuffer[Int]()
    buffer.append(1)
    buffer.append(2)
    buffer.append(3)
    buffer.insert(1, 20)
    buffer.insert(3, -44)

    assert(buffer(0) == 1)
    assert(buffer(1) == 20)
    assert(buffer(2) == 2)
    assert(buffer(3) == -44)
    assert(buffer(4) == 3)
    assert(buffer.length == 5)
  }

  it should "remove elements correctly from the middle of the buffer" in {
    val buffer = new ListBuffer[String]()
    buffer.append("One")
    buffer.append("Two")
    buffer.append("Three")
    buffer.append("Four")
    buffer.append("Five")
    buffer.remove(3)
    buffer.remove(1)

    assert(buffer(0) == "One")
    assert(buffer(1) == "Three")
    assert(buffer(2) == "Five")
    assert(buffer.length == 3)
  }

  it should "reassign existed values with '=' sign" in {
    val buffer = new ListBuffer[String]()
    buffer.append("One")
    buffer.append("Two")
    buffer.append("Three")
    buffer(0) = "Not one"
    buffer(1) = "Not two"
    buffer(2) = "Not three"

    assert(buffer(0) == "Not one")
    assert(buffer(1) == "Not two")
    assert(buffer(2) == "Not three")
  }

  it should "throw an error if the index is out of bounds" in {
    val buffer = new ListBuffer[Int]()
    buffer.append(1)
    buffer.append(2)

    assertThrows[ArrayIndexOutOfBoundsException] {
      buffer(10)
    }
    assertThrows[ArrayIndexOutOfBoundsException] {
      buffer(-1)
    }
  }

  it should "handle corner cases correctly: remove first or last element" in {
    val buffer = new ListBuffer[String]()
    buffer.append("One")
    buffer.append("Two")
    buffer.append("Three")

    buffer.remove(2)
    assert(buffer(0) == "One")
    assert(buffer(1) == "Two")
    assert(buffer.length == 2)

    buffer.remove(0)
    assert(buffer(0) == "Two")
    assert(buffer.length == 1)

    buffer.remove(0)
    assert(buffer.length == 0)
  }

  it should "use 'foreach' method correctly" in {
    val myBuffer = new ListBuffer[Int]()
    myBuffer.append(1)
    myBuffer.append(2)
    myBuffer.append(3)
    myBuffer.append(4)
    myBuffer.append(5)

    val results = Array.ofDim[Int](5)
    myBuffer.foreach(x => results(x - 1) = x * x)

    assert(results(0) == 1)
    assert(results(1) == 4)
    assert(results(2) == 9)
    assert(results(3) == 16)
    assert(results(4) == 25)
  }

  it should "prepend elements in the beginning of a buffer" in {
    val buffer = new ListBuffer[Int]()
    buffer.prepend(1)
    buffer.prepend(2)
    buffer.prepend(3)

    assert(buffer(0) == 3)
    assert(buffer(1) == 2)
    assert(buffer(2) == 1)
    assert(buffer.length == 3)
  }
}
