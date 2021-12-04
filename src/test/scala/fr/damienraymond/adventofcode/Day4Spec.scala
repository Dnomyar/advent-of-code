package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.adventofcode.Day4.BingoGrid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {

  it should "mark not mark as won for a grip of i" in {
    BingoGrid.fromString("1").get.hasWon should be (false)
  }

  it should "mark as won for a grid of 1 of number is called" in {
    val Some(grid) = BingoGrid.fromString("1")
    grid.calledNumber(1).hasWon should be (true)
  }

  it should "not mark as won for a grid of 2 if only one number is called" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    grid.calledNumber(1).hasWon should be (false)
  }

  it should "mark as won for a grid of 2 if only all numbers of a line are called" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    grid.calledNumber(1).calledNumber(2).hasWon should be (true)
    grid.calledNumber(3).calledNumber(4).hasWon should be (true)
  }

  it should "not mark as won for a grid of 2 if numbers from the diagonal are called" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    grid.calledNumber(1).calledNumber(4).hasWon should be (false)
    grid.calledNumber(2).calledNumber(3).hasWon should be (false)
  }

  it should "mark as won for a grid of 2 if from columns are called" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    grid.calledNumber(1).calledNumber(3).hasWon should be (true)
    grid.calledNumber(2).calledNumber(4).hasWon should be (true)
  }

  it should "return the sum unmarked numbers 1" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    Day4.programPart1(grid, List(1,2,3,4)) should contain ((3 + 4) * 2)
  }

  it should "return the sum unmarked numbers 2" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    Day4.programPart1(grid, List(1,3,4,2)) should contain ((2 + 4) * 3)
  }

  it should "empty if the grid did not won" in {
    val Some(grid) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    Day4.programPart1(grid, List.empty) should be (empty)
    Day4.programPart1(grid, List(5,6,7,8,9)) should be (empty)
  }

  it should "return the sum unmarked numbers for the grid that won" in {
    val Some(grid1) = BingoGrid.fromString(
      """1 2
        |3 4""".stripMargin)
    val Some(grid2) = BingoGrid.fromString(
      """5 6
        |7 8""".stripMargin)
    Day4.programPart1(Vector(grid1, grid2), List(1,6,7,3,4,9)) should contain ((2+4)*3)
  }


  it should "parse and execute the program" in {
    val input = """1,6,7,3,4,9
                  |
                  |1 2
                  |3 4
                  |
                  |5 6
                  |7 8""".stripMargin

    Day4.parseAndProgramPart1(input) should contain ((2+4)*3)
  }



  it should "parse and execute the program for the example" in {
    val input = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
                  |
                  |22 13 17 11  0
                  | 8  2 23  4 24
                  |21  9 14 16  7
                  | 6 10  3 18  5
                  | 1 12 20 15 19
                  |
                  | 3 15  0  2 22
                  | 9 18 13 17  5
                  |19  8  7 25 23
                  |20 11 10 24  4
                  |14 21 16 12  6
                  |
                  |14 21 17 24  4
                  |10 16 15  9 19
                  |18  8 23 26 20
                  |22 11 13  6  5
                  | 2  0 12  3  7""".stripMargin

    Day4.parseAndProgramPart1(input) should contain (4512)
  }


  it should "return return the actual result for the all input part1" in {
    val res = Day4.day4Input.use(lines =>
      IO(Day4.parseAndProgramPart1(lines.mkString("\n")))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

}
