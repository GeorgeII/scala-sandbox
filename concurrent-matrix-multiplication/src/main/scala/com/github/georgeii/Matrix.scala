package com.github.georgeii

import scala.math.Numeric.Implicits._
import scala.reflect.ClassTag

case class Matrix[N : Numeric : ClassTag](matrix: Vector[Vector[N]]) {

  def getRow(idx: Int): Vector[N] =
    matrix(idx)

  def getColumn(idx: Int): Vector[N] =
    matrix.map(row => row(idx))

  def transpose: Matrix[N] = {
    val newMatrix = {
      for{
        i <- 0 until this.shape._2
      } yield matrix.map{row => row(i)}
    }.toVector

    Matrix[N](newMatrix)
  }

  def *(other: Matrix[N]): Matrix[Double] = {
    if (shape._2 != other.shape._1)
      throw new IllegalArgumentException("Matrices of these shapes cannot be multiplied")

    val result = Array.ofDim[Double](shape._1, other.shape._2)

    for (i <- 0 until shape._1; j <- 0 until other.shape._2) {
      var aggr = 0
      val thisRow = this.getRow(i)
      val otherColumn = other.getColumn(j)
      result(i)(j) = {
        for{
          k <- 0 until shape._2
        } yield thisRow(k) * otherColumn(k)
      }.sum.toDouble
    }

    Matrix(result.map(_.toVector).toVector)
  }

  def shape: (Int, Int) = (matrix.size, matrix(0).size)

  override def toString: String = s"Matrix of $matrix:\n" + matrix.map{_.mkString("\t")}.mkString("\n")
}



