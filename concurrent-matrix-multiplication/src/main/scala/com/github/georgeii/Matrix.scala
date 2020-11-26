package com.github.georgeii

import com.github.georgeii.Matrix.multiplyRowByColumn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
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

  def multiplyConcurrently(other: Matrix[N], numberOfThreads: Int = 2): Matrix[Double] = {
    if (shape._2 != other.shape._1)
      throw new IllegalArgumentException("Matrices of these shapes cannot be multiplied")

    val resultMatrix = Await.result(createFutureWithRowsRange(other, 0, shape._1 - 1), 20 second)

    Matrix(resultMatrix)
  }

  /**
   * Each Future represents a multiplication of rows from 'startFromRow' to 'endAtRow' by every column of the right matrix.
   * @param startFromRow Future starts calculating multiplication from this row of the left matrix.
   * @param endAtRow Future ends calculating multiplication at this row of the right matrix.
   * @return Submatrix of a result multiplication matrix.
   */
  private def createFutureWithRowsRange(other: Matrix[N], startFromRow: Int, endAtRow: Int): Future[Vector[Vector[Double]]] = {
    val resultSubmatrix = Future {
      {for {
        i <- startFromRow to endAtRow
      } yield
        {
          val row = getColumn(i)

          {
            for {
              j <- 0 until other.shape._2
            } yield multiplyRowByColumn(row, other.getColumn(j))
          }.toVector
        }
      }.toVector
    }

    resultSubmatrix
  }

  def shape: (Int, Int) = (matrix.size, matrix(0).size)

  override def toString: String = s"Matrix of $matrix:\n" + matrix.map{_.mkString("\t")}.mkString("\n")
}

object Matrix {

  def multiplyRowByColumn[N : Numeric : ClassTag](row: Vector[N], column: Vector[N]): Double = {
    val scalarProductOfVectors = (row, column).zipped.map(_ * _).sum

    scalarProductOfVectors.toDouble
  }
}
