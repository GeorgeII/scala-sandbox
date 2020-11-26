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

  it should "show matrix shape" in {
    val arr = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(10, 11, 12))
    val matrix = Matrix(arr)

    assert(matrix.shape._1 == 4)
    assert(matrix.shape._2 == 3)
  }

  it should "override toString method" in {
    val arr = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9), Vector(10, 11, 12))
    val matrix = Matrix(arr)
    println(matrix)
    println(matrix.transpose)
  }

  it should "multiply matrices" in {
    val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val matrix2 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val result1 = matrix1 * matrix2
    assert(result1 == Matrix(Vector(Vector(30.0, 36.0, 42.0), Vector(66.0, 81.0, 96.0), Vector(102.0, 126.0, 150.0))))

    val matrix3 = Matrix(Vector(Vector(1.0, 2, 3), Vector(4.0, 5, 6), Vector(7.0, 8, 9), Vector(10.0, 11, 12)))
    val matrix4 = Matrix(Vector(Vector(4.0, 5, 7, 8), Vector(15.0, 14, 13, 12), Vector(1.0, 2, 3, 4)))
    val result2 = matrix3 * matrix4
    assert(result2 == Matrix(
        Vector(Vector(37.0, 39.0, 42.0, 44.0), Vector(97.0, 102.0, 111.0, 116.0),
        Vector(157.0, 165.0, 180.0, 188.0), Vector(217.0, 228.0, 249.0, 260.0))
      )
    )
  }

  it should "throw an error when wrong size matrices are multiplied" in {
    val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6)))
    val matrix2 = Matrix(Vector(Vector(1, 2, 3), Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val matrix3 = Matrix(Vector(Vector(7.0, 8, 9), Vector(10.0, 11, 12)))
    val matrix4 = Matrix(Vector(Vector(15.0, 14, 13, 12), Vector(1.0, 2, 3, 4)))

    assertThrows[IllegalArgumentException] {
      matrix1 * matrix2
    }
    assertThrows[IllegalArgumentException] {
      matrix3 * matrix4
    }
  }

  it should "run with one Future" in {
    val matrix1 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val matrix2 = Matrix(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)))
    val result1 = matrix1.multiplyConcurrently(matrix2)
    assert(result1 == Matrix(Vector(Vector(30.0, 36.0, 42.0), Vector(66.0, 81.0, 96.0), Vector(102.0, 126.0, 150.0))))
  }






  "multiplyRowByColumn" should "return a scalar multiplication of two vectors" in {
    val vec1 = Vector(1, 2, 3)
    val vec2 = Vector(4, 5, 6)
    val res1 = Matrix.multiplyRowByColumn(vec1, vec2)

    assert(res1 == 32.0)
  }
}
