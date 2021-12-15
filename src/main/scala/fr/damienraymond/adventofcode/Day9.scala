package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day9 {

  def part1(stripMargin: String): Int =
    (parse _ andThen findLowPoints andThen part1Result)(stripMargin)

  def parse(string: String): Vector[Vector[Int]] = {
    string.split("\n").map(_.split("").map(_.toInt).toVector).toVector
  }

  def findLowCoordinates(matrix: Vector[Vector[Int]]): Set[(Int, Int)] = {
    val allCoordinates = (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield (i, j)).toSet

    allCoordinates
      .filter { case (i, j) =>
        findNeighbours(matrix)(i, j).forall(_ > matrix(i)(j))
      }
  }

  def findLowPoints(matrix: Vector[Vector[Int]]): Vector[Int] =
    findLowCoordinates(matrix).toVector.map { case (i, j) =>
      matrix(i)(j)
    }

  def part1Result(lowPoints: Vector[Int]): Int = lowPoints.map(_ + 1).sum

  def findNeighboursCoordinate(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): Set[(Int, Int)] =
    Vector(
      (i - 1, j),
      (i + 1, j),
      (i, j - 1),
      (i, j + 1)
    ).filter {
      case (i, _) if i < 0                 => false
      case (i, _) if i >= matrix.length    => false
      case (_, j) if j < 0                 => false
      case (_, j) if j >= matrix(0).length => false
      case _                               => true
    }.toSet

  def findNeighbours(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): Vector[Int] =
    findNeighboursCoordinate(matrix)(i, j).toVector.map { case (i, j) =>
      matrix(i)(j)
    }

  val input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day9.txt")

  def part2(stripMargin: String): Int =
    (parse _ andThen findBasins andThen part2Result)(stripMargin)

  def part2Result(lowPoints: Vector[Int]): Int = lowPoints.product

  def findBasin(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): Set[(Int, Int)] = {

    def loop(
        coordinatesToVisit: Set[(Int, Int)],
        res: Set[(Int, Int)]
    ): Set[(Int, Int)] = {
      val newCoordinated =
        coordinatesToVisit.flatMap { case (i, j) =>
          findNeighboursCoordinate(matrix)(i, j)
            .diff(res)
            .filter { case (i, j) =>
              matrix(i)(j) < 9
            }
        }

      if (newCoordinated.nonEmpty) loop(newCoordinated, res ++ newCoordinated)
      else res

    }
    loop(Set((i, j)), Set.empty)
  }

  def findBasins(
      matrix: Vector[Vector[Int]]
  ): Vector[Int] = {

    val coordinates = findLowCoordinates(matrix)

    val basins = coordinates.foldLeft(Set.empty[Set[(Int, Int)]]) {
      case (acc, (i, j)) if acc.flatten.contains((i, j)) => acc
      case (acc, (i, j)) =>
        acc + findBasin(matrix)(i, j)
    }

    basins.toList.map(_.size).sorted.reverse.take(3).toVector
  }

}
