package com.github.georgeii

import org.scalatest.flatspec.AnyFlatSpec

class MatrixTest extends AnyFlatSpec {

  "Matrix" should "be created from Vector of Vectors of different types" in {
    val matrixOfInt = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val matrixOfDouble = Matrix(Vector(Vector(1.0, 2.0, 3.0), Vector(4.0, 5.0, 6.0), Vector(7.0, 8.0, 9.0)))
    val shrt: Short = 9
    val matrixOfShort = Matrix(Vector(Vector(shrt, shrt, shrt), Vector(shrt, shrt, shrt), Vector(shrt, shrt, shrt)))
  }

  it should "be created of rectangle form" in {
    val arr = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(10, 11, 12))
    val matrix = Matrix(arr)
  }

  it should "return a transposed matrix" in {
    val arr = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(10, 11, 12))
    val matrix = Matrix(arr)

    val transposed = matrix.transpose
    assert(transposed == Matrix(Vector(Vector(1, 4, 7, 10), Vector(2, 5, 8, 11), Vector(3, 6, 9, 12))))
  }

  it should "return row and column by indices" in {
    val arr = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(10, 11, 12))
    val matrix = Matrix(arr)

    assert(matrix.getColumn(0) == Vector(1, 4, 7, 10))
    assert(matrix.getRow(2) == Vector(7, 8, 9))
  }

}
