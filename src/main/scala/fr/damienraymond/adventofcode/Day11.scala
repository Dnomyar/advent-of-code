package fr.damienraymond.adventofcode

import scala.collection.mutable

object Day11 {
  def part1(input: String): Int =
    (parse _ andThen evolution1(100))(input)._2

  def part2(input: String): Option[Int] =
    (parse _ andThen evolution1(500))(input)._3

  def parse(input: String): Vector[Vector[Int]] =
    input.split("\n").map(_.split("").map(_.toInt).toVector).toVector

  def neighbours(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): List[(Int, Int)] = {
    List(
      (i - 1, j),
      (i + 1, j),
      (i, j - 1),
      (i, j + 1),
      (i - 1, j - 1),
      (i + 1, j + 1),
      (i + 1, j - 1),
      (i - 1, j + 1)
    ).filter {
      case (i, j)
          if i >= 0 && i < matrix.length && j >= 0 && j < matrix(0).length =>
        true
      case _ => false
    }
  }

  def evolution1(numberOfEvolution: Int)(
      graph: Vector[Vector[Int]]
  ): (Vector[Vector[Int]], Int, Option[Int]) = {
    val maybeToFlash = mutable.Queue.empty[(Int, Int)]

    val mutableGraph: mutable.Seq[mutable.ArraySeq[Int]] =
      mutable.ArraySeq.from(
        graph.map(mutable.ArraySeq.from)
      )

    val coordinates = for {
      i <- mutableGraph.indices
      j <- mutableGraph(i).indices
    } yield (i, j)

    val numberOfOctopuses = coordinates.size

    var numberOfBlips = 0

    def handleFlash(
        hasFlashed: mutable.Set[(Int, Int)],
        i: Int,
        j: Int
    ): Unit = {
      if (mutableGraph(i)(j) > 9) {
        hasFlashed.add((i, j))
        numberOfBlips = numberOfBlips + 1
        maybeToFlash.enqueueAll(neighbours(graph)(i, j))
        mutableGraph(i)(j) = 0
      }
    }

    var allOctopusesFlashingAtTheSameTimeIndex = Option.empty[Int]

    for (idx <- 1 to numberOfEvolution) {
      val hasFlashed = mutable.Set.empty[(Int, Int)]
      coordinates.foreach { case (i, j) =>
        mutableGraph(i)(j) = mutableGraph(i)(j) + 1
        handleFlash(hasFlashed, i, j)
      }
      while (maybeToFlash.nonEmpty) {
        val (i, j) = maybeToFlash.dequeue()
        if (!hasFlashed.contains((i, j))) {
          mutableGraph(i)(j) = mutableGraph(i)(j) + 1
        }
        handleFlash(hasFlashed, i, j)
      }
      if (
        numberOfOctopuses == hasFlashed.size && allOctopusesFlashingAtTheSameTimeIndex.isEmpty
      ) {
        allOctopusesFlashingAtTheSameTimeIndex = Some(idx)
      }
    }

    (
      mutableGraph.map(_.to(Vector)).to(Vector),
      numberOfBlips,
      allOctopusesFlashingAtTheSameTimeIndex
    )
  }

}
