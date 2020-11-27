package com.github.georgeii

import com.github.georgeii.Matrix.multiplyRowByColumn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.math.Numeric.Implicits._
import scala.reflect.ClassTag
import scala.util.Random

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

    val resultMatrix = {
      for (i <- 0 until shape._1)
        yield {
          // every row in resultMatrix is calculated in a Future.
          val futures = for (j <- 0 until other.shape._2)
            yield Future {
              val thisRow = this.getRow(i)
              val otherColumn = other.getColumn(j)
              multiplyRowByColumn(thisRow, otherColumn)
            }

          futures.map(Await.result(_, Duration.Inf))
        }
    }

    Matrix(resultMatrix.map(_.toVector).toVector)
  }

  def shape: (Int, Int) = (matrix.size, matrix(0).size)

  override def toString: String = s"Matrix of $matrix:\n" + matrix.map{_.mkString("\t")}.mkString("\n")
}


object Matrix {

  def multiplyRowByColumn[N : Numeric : ClassTag](row: Vector[N], column: Vector[N]): Double = {
    val scalarProductOfVectors = (row, column).zipped.map(_ * _).sum

    scalarProductOfVectors.toDouble
  }

  def createRandomMatrixOfDoubles(rowsNumber: Int, columnsNumber: Int): Matrix[Double] = {
    val randomGenerator = Random
    val result = Array.ofDim[Double](rowsNumber, columnsNumber)

    for{
      i <- 0 until rowsNumber
      j <- 0 until columnsNumber
    } // create a random number in (-500, 500) range.
      result(i)(j) = randomGenerator.nextDouble() * 1000 - 500

    Matrix(result.map(_.toVector).toVector)
  }
}
