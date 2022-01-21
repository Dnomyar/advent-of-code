package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.ReadFileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class Day14Spec extends AnyFlatSpec with Matchers {

  it should "part1 example" in {
    Day14.part1("""NNCB
                  |
                  |CH -> B
                  |HH -> N
                  |CB -> H
                  |NH -> C
                  |HB -> C
                  |HC -> B
                  |HN -> C
                  |NN -> C
                  |BH -> H
                  |NC -> B
                  |NB -> B
                  |BN -> B
                  |BB -> N
                  |BC -> B
                  |CC -> N
                  |CN -> C""".stripMargin.split("\n").to(LazyList)) should be(
      1588
    )
  }

  it should "parse" in {
    Day14.parse("""NNCB
                  |
                  |CH -> B
                  |HH -> N
                  |CB -> H
                  |NH -> C
                  |HB -> C
                  |HC -> B
                  |HN -> C
                  |NN -> C
                  |BH -> H
                  |NC -> B
                  |NB -> B
                  |BN -> B
                  |BB -> N
                  |BC -> B
                  |CC -> N
                  |CN -> C""".stripMargin.split("\n").to(LazyList)) should be(
      (
        "NNCB",
        Map(
          ('C', 'H') -> 'B',
          ('H', 'H') -> 'N',
          ('C', 'B') -> 'H',
          ('N', 'H') -> 'C',
          ('H', 'B') -> 'C',
          ('H', 'C') -> 'B',
          ('H', 'N') -> 'C',
          ('N', 'N') -> 'C',
          ('B', 'H') -> 'H',
          ('N', 'C') -> 'B',
          ('N', 'B') -> 'B',
          ('B', 'N') -> 'B',
          ('B', 'B') -> 'N',
          ('B', 'C') -> 'B',
          ('C', 'C') -> 'N',
          ('C', 'N') -> 'C'
        )
      )
    )
  }

  it should "part 1" in {
    val res = ReadFileUtil
      .readFileLine("day14.txt")
      .use(line => IO(Day14.part1(line)))
      .unsafeRunSync()
    res should be(2223)
    println(s"part1 ${res}")
  }

  it should "part 2" in {
    val res = ReadFileUtil
      .readFileLine("day14.txt")
      .use(line => IO(Day14.part2(line)))
      .unsafeRunSync()
    res should be(2566282754493L)
    println(s"part2 ${res}")
  }

}
