package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9Spec extends AnyFlatSpec with Matchers {

  it should "work with the example part 1" in {
    Day9.part1(
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678""".stripMargin
    ) should be(15)
  }

  val matrix = Vector(
    Vector(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
    Vector(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
    Vector(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
    Vector(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
    Vector(9, 8, 9, 9, 9, 6, 5, 6, 7, 8)
  )

  it should "find low points" in {
    Day9.findLowPoints(matrix) should contain theSameElementsAs Vector(
      1,
      0,
      5,
      5
    )
  }

  it should "find neighbours in the middle" in {
    Day9.findNeighbours(matrix)(3, 1) should contain theSameElementsAs Vector(
      8,
      8,
      8,
      6
    )
  }

  it should "find neighbours top left hand corner" in {
    Day9.findNeighbours(matrix)(0, 0) should contain theSameElementsAs Vector(
      1,
      3
    )
  }

  it should "find neighbours top right hand corner" in {
    Day9.findNeighbours(matrix)(0, 9) should contain theSameElementsAs Vector(
      1,
      1
    )
  }

  it should "find neighbours bottom right hand corner" in {
    Day9.findNeighbours(matrix)(4, 9) should contain theSameElementsAs Vector(
      9,
      7
    )
  }

  it should "find neighbours bottom left hand corner" in {
    Day9.findNeighbours(matrix)(4, 0) should contain theSameElementsAs Vector(
      8,
      8
    )
  }

  it should "return print the result for part1" in {
    val res = Day9.input
      .use(lines => IO(Day9.part1(lines.mkString("\n"))))
      .unsafeRunSync()
    println(s"Part 1 result ${res}")
  }
  it should "return print the result for part2" in {
    val res = Day9.input
      .use(lines => IO(Day9.part2(lines.mkString("\n"))))
      .unsafeRunSync()
    println(s"Part 2 result ${res}")
  }

  it should "work with the example part 2" in {
    Day9.part2(
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678""".stripMargin
    ) should be(1134)
  }

  it should "find basin 1" in {
    Day9.findBasin(matrix)(0, 1) should be(Set((0, 1), (0, 0), (1, 0)))
  }

}
