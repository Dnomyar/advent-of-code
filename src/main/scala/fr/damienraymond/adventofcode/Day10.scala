package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

import scala.annotation.tailrec

object Day10 {

  def part1(lines: List[String]): Int =
    part1Score(lines.flatMap(findIncorrectClosingChars))

  def findIncorrectClosingChars(line: String): Option[Char] = {

    val openingChar = Map(
      ']' -> '[',
      '}' -> '{',
      ')' -> '(',
      '>' -> '<'
    )

    val isOpeningChar = (char: Char) => openingChar.values.toSet.contains(char)

    @tailrec
    def loop(
        input: List[Char],
        queue: List[Char],
        incorrectChars: List[Char]
    ): Option[Char] = input match {
      case head :: tail if isOpeningChar(head) =>
        loop(tail, head :: queue, incorrectChars)
      case head :: tail if queue.headOption.contains(openingChar(head)) =>
        loop(tail, queue.tail, incorrectChars)
      case head :: _ => Some(head)
      case Nil =>
        None
    }

    loop(line.toCharArray.toList, List.empty, List.empty)
  }

  def part1Score(incorrectChars: List[Char]): Int = incorrectChars.map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum

  val input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day10.txt")

}
