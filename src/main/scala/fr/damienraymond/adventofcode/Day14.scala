package fr.damienraymond.adventofcode

import scala.annotation.tailrec

object Day14 {
  def part1(input: LazyList[String]): Long =
    (parse _ andThen derive(10))(input)

  def part2(input: LazyList[String]): Long =
    (parse _ andThen derive(40))(input)

  def parse(input: LazyList[String]): (String, Map[(Char, Char), Char]) =
    input.toList.span(_ != "") match {
      case (List(template), _ :: map) =>
        (
          template,
          map.map { case s"${pair} -> ${element}" =>
            (pair(0), pair(1)) -> element(0)
          }.toMap
        )
    }

  def derive(numberOrDerivation: Int)(
      input: (String, Map[(Char, Char), Char])
  ): Long = {

    val (initTemplate, rules) = input

    def initialNumberOfPairFromTemplate(
        initTemplate: String
    ): Map[(Char, Char), Long] = {
      initTemplate
        .sliding(2)
        .map(pair => (pair(0), pair(1)))
        .foldLeft(Map.empty[(Char, Char), Long].withDefaultValue(0L)) {
          case (acc, pair) =>
            acc.updated(pair, acc(pair) + 1L)
        }
    }

    @tailrec
    def countNumberPairLoop(n: Int)(
        previousNumberOfPairs: Map[(Char, Char), Long]
    ): Map[(Char, Char), Long] = {
      if (n < 1) previousNumberOfPairs
      else {
        val currentNumberOfPairs =
          previousNumberOfPairs.foldLeft(
            Map.empty[(Char, Char), Long].withDefaultValue(0L)
          ) { case (currentNumberOfPairs, (pair @ (a, c), number)) =>
            val b = rules(pair)
            currentNumberOfPairs
              .updated(
                (a, b),
                currentNumberOfPairs((a, b)) + (1L * number)
              )
              .updated(
                (b, c),
                currentNumberOfPairs((b, c)) + (1L * number)
              )

          }
        countNumberPairLoop(n - 1)(currentNumberOfPairs)
      }
    }

    def computerNumberOfElements(
        currentNumberOfPairs: Map[(Char, Char), Long]
    ): Map[Char, Long] = {
      currentNumberOfPairs.foldLeft(
        Map(initTemplate.head -> 1L).withDefaultValue(0L)
      ) { case (numberOfElements, ((_, element), number)) =>
        numberOfElements.updated(element, numberOfElements(element) + number)
      }
    }

    def computeScore(numberOfElements: Map[Char, Long]): Long = {
      val max = numberOfElements.values.toSet.max
      val min = numberOfElements.values.toSet.min
      max - min
    }

    (initialNumberOfPairFromTemplate _)
      .andThen(countNumberPairLoop(numberOrDerivation))
      .andThen(computerNumberOfElements)
      .andThen(computeScore)(initTemplate)
  }

}
