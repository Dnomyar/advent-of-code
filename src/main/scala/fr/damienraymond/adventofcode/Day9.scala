package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day9 {
  def part1(stripMargin: String): Int =
    (parse _ andThen findLowPoints andThen part1Result)(stripMargin)

  def parse(string: String): Vector[Vector[Int]] = {
    string.split("\n").map(_.split("").map(_.toInt).toVector).toVector
  }

  def findLowPoints(matrix: Vector[Vector[Int]]): Vector[Int] = {
    val findNeighboursMatrix = findNeighbours(matrix) _
    matrix.indices.flatMap { lineIdx =>
      (matrix(lineIdx).indices).flatMap { colIdx =>
        val current = matrix(lineIdx)(colIdx)
        val neighbours = findNeighboursMatrix(lineIdx, colIdx)
        val bool = neighbours.forall(_ > current)
        if (bool) Some(current)
        else None
      }
    }.toVector
  }

  def part1Result(lowPoints: Vector[Int]) = lowPoints.map(_ + 1).sum

  def findNeighbours(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): Vector[Int] =
    Vector(
      (i - 1, j),
      (i + 1, j),
      (i, j - 1),
      (i, j + 1)
    ).filter {
      case (i, j) if i < 0                 => false
      case (i, j) if i >= matrix.length    => false
      case (i, j) if j < 0                 => false
      case (i, j) if j >= matrix(0).length => false
      case _                               => true
    }.map { case (i, j) =>
      matrix(i)(j)
    }

  val input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day9.txt")

}
