package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil
import fr.damienraymond.adventofcode.Coordinates.{Coordinates, CoordinatesPart1, CoordinatesPart2}

object Day2 {

  sealed trait Command

  object Command {
    case class Forward(value: Int) extends Command

    case class Down(value: Int) extends Command

    case class Up(value: Int) extends Command
  }

  def parse(input: String): Option[Command] =
    input match {
      case s"forward ${value}" => value.toIntOption.map(Command.Forward)
      case s"down ${value}" => value.toIntOption.map(Command.Down)
      case s"up ${value}" => value.toIntOption.map(Command.Up)
    }

  def coordinateToProblemResult(coordinates: Coordinates): Int =
    coordinates match {
      case CoordinatesPart1(horizontal, depth) => horizontal * depth
      case CoordinatesPart2(aim, horizontal, depth) => horizontal * depth
    }

  def applyCommands(initialCoordinates: Coordinates)(commands: LazyList[Command]): Coordinates = {
    def applyCommand(coordinates: Coordinates, command: Command): Coordinates =
      command match {
        case Command.Forward(value) => coordinates.forward(value)
        case Command.Down(value) => coordinates.down(value)
        case Command.Up(value) => coordinates.up(value)
      }

    commands.foldLeft(initialCoordinates)(applyCommand)
  }

  def program(initialCoordinates: Coordinates)(input: LazyList[String]): Int = {
    val commands = input.flatMap(parse)
    val coordinates = applyCommands(initialCoordinates)(commands)
    coordinateToProblemResult(coordinates)
  }

  def day2Part1(input: LazyList[String]): Int = program(CoordinatesPart1.initial)(input)

  def day2Part2(input: LazyList[String]): Int = program(CoordinatesPart2.initial)(input)

  val day2Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day2.txt")


}

object Coordinates {

  trait Coordinates {
    def forward(value: Int): Coordinates

    def down(value: Int): Coordinates

    def up(value: Int): Coordinates
  }

  case class CoordinatesPart1(horizontal: Int, depth: Int) extends Coordinates {
    def forward(value: Int): CoordinatesPart1 =
      copy(horizontal = horizontal + value)

    def down(value: Int): CoordinatesPart1 =
      copy(depth = depth + value)

    def up(value: Int): CoordinatesPart1 =
      copy(depth = depth - value)
  }

  object CoordinatesPart1 {
    val initial: CoordinatesPart1 = CoordinatesPart1(0, 0)
  }

  case class CoordinatesPart2(aim: Int, horizontal: Int, depth: Int) extends Coordinates {
    def forward(value: Int): CoordinatesPart2 =
      copy(
        horizontal = horizontal + value,
        depth = depth + aim * value
      )

    def down(value: Int): CoordinatesPart2 =
      copy(aim = aim + value)

    def up(value: Int): CoordinatesPart2 =
      copy(aim = aim - value)
  }

  object CoordinatesPart2 {
    val initial: CoordinatesPart2 = CoordinatesPart2(0, 0, 0)
  }

}

