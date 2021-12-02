package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.adventofcode.Day2.{Command, Coordinates}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {

  private val applyCommands: LazyList[Command] => Coordinates = Day2.applyCommands(Coordinates.initial)

  it should "move the submarine forward by 10" in {
    applyCommands(LazyList(Command.Forward(10))) should be (Coordinates(10, 0))
  }

  it should "move the submarine forward by 5" in {
    applyCommands(LazyList(Command.Forward(5))) should be (Coordinates(5, 0))
  }

  it should "increase the depth of the sub by 4" in {
    applyCommands(LazyList(Command.Down(4))) should be (Coordinates(0, 4))
  }

  it should "increase the depth of the sub by 6" in {
    applyCommands(LazyList(Command.Down(6))) should be (Coordinates(0, 6))
  }

  it should "decrease the depth of the sub by 6" in {
    applyCommands(LazyList(Command.Up(6))) should be (Coordinates(0, -6))
  }

  it should "decrease the depth of the sub by 9" in {
    applyCommands(LazyList(Command.Up(9))) should be (Coordinates(0, -9))
  }

  it should "apply multiple commands" in {
    applyCommands(LazyList(
      Command.Forward(5),
      Command.Down(5),
      Command.Forward(8),
      Command.Up(3),
      Command.Down(8),
      Command.Forward(2)
    )) should be (Coordinates(15, 10))
  }

  it should "return the right result" in {
    Day2.coordinateToProblemResult(Coordinates(15, 10)) should be (150)
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

  it should "return 150 for the all example" in {
    Day2.day2(
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin.split("\n").to(LazyList)
    ) should be (150)
  }


  it should "return return the actual result for the all input" in {
    val res = Day2.day2Input.use(lines => IO(Day2.day2(lines))).unsafeRunSync()
    println(res)
  }

}
