package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.adventofcode.Day5.Line
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {
  val testInput =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.split("\n").to(LazyList)

  it should "count the number of line intersections" in {
    Day5.part1(testInput) should be (5)
  }

  it should "count the number of intersections of two lines - 1" in {
    Day5.countIntersection(LazyList(
      Line(Day5.Coordinates(0,0), Day5.Coordinates(10,0)),
      Line(Day5.Coordinates(0,0), Day5.Coordinates(0,10))
    ).flatten) should be (1)
  }


  it should "count the number of intersections of two lines - 2" in {
    Day5.countIntersection(LazyList(
      Line(Day5.Coordinates(0,0), Day5.Coordinates(1,0)),
      Line(Day5.Coordinates(0,0), Day5.Coordinates(1,0))
    ).flatten) should be (2)
  }

  it should "return all point of a line - one point" in {
    Line(Day5.Coordinates(0,0), Day5.Coordinates(0,0)).get.allPoints should be (LazyList(Day5.Coordinates(0,0)))
  }

  it should "return all point of a line - x changes" in {
    Line(Day5.Coordinates(0,0), Day5.Coordinates(4,0)).get.allPoints should be (LazyList(
      Day5.Coordinates(0,0),
      Day5.Coordinates(1,0),
      Day5.Coordinates(2,0),
      Day5.Coordinates(3,0),
      Day5.Coordinates(4,0)
    ))
  }

  it should "return all point of a line - y changes" in {
    Line(Day5.Coordinates(0,5), Day5.Coordinates(0,7)).get.allPoints should be (LazyList(
      Day5.Coordinates(0,5),
      Day5.Coordinates(0,6),
      Day5.Coordinates(0,7)
    ))
  }

  it should "parse input" in {
    Day5.parse(
      """0,9 -> 5,9
        |0,0 -> 8,0""".stripMargin.split("\n").to(LazyList)
    ) should be (LazyList(
      Line(Day5.Coordinates(0,9), Day5.Coordinates(5,9)),
      Line(Day5.Coordinates(0,0), Day5.Coordinates(8,0))
    ).flatten)
  }

  it should "return print the result for part1" in {
    val res = Day5.day5Input.use(lines =>
      IO(Day5.part1(lines))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

  "Diagonal45Line#allPoints" should "return all point 1" in {
    Line(Day5.Coordinates(0,0), Day5.Coordinates(4,4)).get.allPoints should be (LazyList(
      Day5.Coordinates(0,0),
      Day5.Coordinates(1,1),
      Day5.Coordinates(2,2),
      Day5.Coordinates(3,3),
      Day5.Coordinates(4,4)
    ))
  }

  it should "return all point 2" in {
    Line(Day5.Coordinates(4,0), Day5.Coordinates(0,4)).get.allPoints should be (LazyList(
      Day5.Coordinates(4,0),
      Day5.Coordinates(3,1),
      Day5.Coordinates(2,2),
      Day5.Coordinates(1,3),
      Day5.Coordinates(0,4)
    ))
  }

  it should "count intersections of diagonal lines" in {
    Day5.countIntersection(LazyList(
      Line(Day5.Coordinates(0,0), Day5.Coordinates(10,10)),
      Line(Day5.Coordinates(10,0), Day5.Coordinates(0,10))
    ).flatten) should be (1)
  }

  it should "count the number of line intersections" in {
    Day5.part2(testInput) should be (12)
  }

  it should "return print the result for part1" in {
    val res = Day5.day5Input.use(lines =>
      IO(Day5.part2(lines))
    ).unsafeRunSync()
    println(s"Part 2 result ${res}")
  }

}
