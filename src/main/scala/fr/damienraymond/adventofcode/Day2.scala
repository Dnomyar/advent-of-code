package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day2 {

  sealed trait Command

  object Command {
    case class Forward(value: Int) extends Command

    case class Down(value: Int) extends Command

    case class Up(value: Int) extends Command
  }

  case class Coordinates(horizontal: Int, depth: Int) {
    def forward(value: Int): Coordinates =
      copy(horizontal = horizontal + value)
    def down(value: Int): Coordinates =
      copy(depth = depth + value)
    def up(value: Int): Coordinates =
      copy(depth = depth - value)
  }

  object Coordinates {
    val initial: Coordinates = Coordinates(0, 0)
  }

  def parse(input: String): Option[Command] =
    input match {
      case s"forward ${value}" => value.toIntOption.map(Command.Forward)
      case s"down ${value}" => value.toIntOption.map(Command.Down)
      case s"up ${value}" => value.toIntOption.map(Command.Up)
    }

  def applyCommands(coordinates: Coordinates, command: Command): Coordinates = command match {
    case Command.Forward(value) => coordinates.forward(value)
    case Command.Down(value) => coordinates.down(value)
    case Command.Up(value) => coordinates.up(value)
  }

  def applyCommands(initialCoordinates: Coordinates)(command: LazyList[Command]): Coordinates =
    command.foldLeft(initialCoordinates)(applyCommands)

  def coordinateToProblemResult(coordinates: Coordinates): Int =
    coordinates match {
      case Coordinates(horizontal, depth) => horizontal * depth
    }


  def day2(input: LazyList[String]): Int = {
    val commands = input.flatMap(parse)
    val coordinates = applyCommands(Coordinates.initial)(commands)
    coordinateToProblemResult(coordinates)
  }


  val day2Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day2.txt")


}

