package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.adventofcode.Coordinates.{Coordinates, CoordinatesPart1, CoordinatesPart2}
import fr.damienraymond.adventofcode.Day2.Command
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {

  private val applyCommandsPart1: LazyList[Command] => Coordinates =
    Day2.applyCommands(CoordinatesPart1.initial)

  private val applyCommandsPart2: LazyList[Command] => Coordinates =
    Day2.applyCommands(CoordinatesPart2.initial)

  it should "move the submarine forward by 10" in {
    applyCommandsPart1(LazyList(Command.Forward(10))) should be (CoordinatesPart1(10, 0))
  }

  it should "move the submarine forward by 5" in {
    applyCommandsPart1(LazyList(Command.Forward(5))) should be (CoordinatesPart1(5, 0))
  }

  it should "increase the depth of the sub by 4" in {
    applyCommandsPart1(LazyList(Command.Down(4))) should be (CoordinatesPart1(0, 4))
  }

  it should "increase the depth of the sub by 6" in {
    applyCommandsPart1(LazyList(Command.Down(6))) should be (CoordinatesPart1(0, 6))
  }

  it should "decrease the depth of the sub by 6" in {
    applyCommandsPart1(LazyList(Command.Up(6))) should be (CoordinatesPart1(0, -6))
  }

  it should "decrease the depth of the sub by 9" in {
    applyCommandsPart1(LazyList(Command.Up(9))) should be (CoordinatesPart1(0, -9))
  }

  it should "apply multiple commands" in {
    applyCommandsPart1(LazyList(
      Command.Forward(5),
      Command.Down(5),
      Command.Forward(8),
      Command.Up(3),
      Command.Down(8),
      Command.Forward(2)
    )) should be (CoordinatesPart1(15, 10))
  }

  it should "return the right result" in {
    Day2.coordinateToProblemResult(CoordinatesPart1(15, 10)) should be (150)
  }

  it should "parse a forward 10 command" in {
    Day2.parse("forward 10") should contain (Command.Forward(10))
  }

  it should "parse a forward 5 command" in {
    Day2.parse("forward 5") should contain (Command.Forward(5))
  }

  it should "parse a down 9 command" in {
    Day2.parse("down 9") should contain (Command.Down(9))
  }

  it should "parse a up 2 command" in {
    Day2.parse("up 2") should contain (Command.Up(2))
  }

  it should "return 150 for the all example for part1" in {
    Day2.day2Part1(
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin.split("\n").to(LazyList)
    ) should be (150)
  }


  it should "return return the actual result for the all input part1" in {
    val res = Day2.day2Input.use(lines => IO(Day2.day2Part1(lines))).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

  it should "increase the aim by 5" in {
    applyCommandsPart2(LazyList(Command.Down(5))) should be (CoordinatesPart2(5, 0, 0))
  }

  it should "increase the aim by 5 + 2" in {
    applyCommandsPart2(LazyList(
      Command.Down(5),
      Command.Down(2),
    )) should be (CoordinatesPart2(7, 0, 0))
  }

  it should "decrease the aim by 9" in {
    applyCommandsPart2(LazyList(Command.Up(9))) should be (CoordinatesPart2(-9, 0, 0))
  }

  it should "decrease the aim by 9+2" in {
    applyCommandsPart2(LazyList(
      Command.Up(9),
      Command.Up(2)
    )) should be (CoordinatesPart2(-11, 0, 0))
  }

  it should "forward the sub by 7" in {
    applyCommandsPart2(LazyList(Command.Forward(7))) should be (CoordinatesPart2(0, 7, 0))
  }

  it should "forward the sub by 7+5" in {
    applyCommandsPart2(LazyList(
      Command.Forward(7),
      Command.Forward(5),
    )) should be (CoordinatesPart2(0, 12, 0))
  }

  it should "forward the sub by 7 with aim of 5" in {
    applyCommandsPart2(LazyList(
      Command.Down(5),
      Command.Forward(7),
    )) should be (CoordinatesPart2(5, 7, 5 * 7))
  }

  it should "forward the sub by 7 and 3 with aim of 5" in {
    applyCommandsPart2(LazyList(
      Command.Down(5),
      Command.Forward(7),
      Command.Forward(3),
    )) should be (CoordinatesPart2(5, 7 + 3, 5 * 7 + 3 * 5))
  }

  it should "return 900 for the all example for part2" in {
    Day2.day2Part2(
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin.split("\n").to(LazyList)
    ) should be (900)
  }

  it should "return return the actual result for the all input part2" in {
    val res = Day2.day2Input.use(lines => IO(Day2.day2Part2(lines))).unsafeRunSync()
    println(s"Part 2 result ${res}")
  }


}
