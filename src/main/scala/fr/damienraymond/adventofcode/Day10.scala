package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

import scala.annotation.tailrec

object Day10 {

  def part1(lines: List[String]): Long =
    part1Score(lines.flatMap(findIncorrectClosingChars))

  def part2(lines: List[String]): Long = {
    val sorted = lines
      .flatMap(findNotClosedChars)
      .map(computeCostOfNotClosedChars)
      .sorted

    sorted(sorted.length / 2)
  }

  private val openingChar = Map(
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    '>' -> '<'
  )

  private val isOpeningChar = (char: Char) =>
    openingChar.values.toSet.contains(char)

  @tailrec
  private def incorrectClosingCharOrNotClosedChars(
      input: List[Char],
      queue: List[Char]
  ): Either[Char, List[Char]] = input match {
    case head :: tail if isOpeningChar(head) =>
      incorrectClosingCharOrNotClosedChars(tail, head :: queue)
    case head :: tail if queue.headOption.contains(openingChar(head)) =>
      incorrectClosingCharOrNotClosedChars(tail, queue.tail)
    case head :: _ => Left(head)
    case Nil =>
      Right(queue)
  }

  def findIncorrectClosingChars(line: String): Option[Char] =
    incorrectClosingCharOrNotClosedChars(
      line.toCharArray.toList,
      List.empty
    ).swap.toOption

  def findNotClosedChars(line: String): Option[List[Char]] =
    incorrectClosingCharOrNotClosedChars(
      line.toCharArray.toList,
      List.empty
    ).toOption

  def computeCostOfNotClosedChars(notClosedChar: List[Char]): Long = {

    val charScore = Map(
      '(' -> 1,
      '[' -> 2,
      '{' -> 3,
      '<' -> 4
    )

    notClosedChar.foldLeft(0L) { case (acc, char) =>
      acc * 5 + charScore(char)
    }
  }

  def part1Score(incorrectChars: List[Char]): Long = incorrectChars.map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum

  val input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day10.txt")

}
