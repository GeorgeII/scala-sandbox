package com.github.georgeii

case class Matrix(matrix: Vector[Vector[_ >: Numeric[_]]]) {

  def getRow(idx: Int): Vector[_] = matrix(idx)

  def getColumn(idx: Int): Vector[_] = {
    for{
      row <- matrix
    } yield row(idx)
  }

  def transpose: Matrix = {
    val newMatrix = {
      for {
        i <- 0 until this.shape._2
      } yield matrix.map{row => row(i)}
    }.toVector

    Matrix(newMatrix)
  }

  def shape: (Int, Int) = (matrix.size, matrix(0).size)
}



