package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  val input =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin.split("\n").to(LazyList)


  "the example" should "yield 198" in {
    Day3.programPart1(input) should be (198)
  }


  "mostCommonBit" should "return 0 for List(0,0)" in {
    Day3.countBits(LazyList("0", "0")).mostCommonBits should be ("0")
  }

  it should "return 1 for List(1,1)" in {
    Day3.countBits(LazyList("1", "1")).mostCommonBits should be ("1")
  }

  it should "return 0 for List(1,0,0)" in {
    Day3.countBits(LazyList("1", "0", "0")).mostCommonBits should be ("0")
  }

  it should "return 101 for List(100,011,101)" in {
    Day3.countBits(LazyList("100", "011", "101")).mostCommonBits should be ("101")
  }

  "lessCommonBit" should "return 010 for List(100,011,101)" in {
    Day3.countBits(LazyList("100", "011", "101")).lessCommonBits should be ("010")
  }

  it should "return return the actual result for the all input part1" in {
    val res = Day3.day3Input.use(lines => IO(Day3.programPart1(lines))).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }


  it should "filter according to the most used bit 1" in {
    Day3.oxygen(LazyList("00", "00", "10")) should be (LazyList("00", "00"))
  }

  it should "filter according to the most used bit in depth" in {
    Day3.oxygen(LazyList("00", "01", "01", "10")) should be (LazyList("01", "01"))
  }

  "the example" should "yield 230 for part 2" in {
    Day3.programPart2(input) should be (230)
  }


  it should "return return the actual result for the all input part2" in {
    val res = Day3.day3Input.use(lines => IO(Day3.programPart2(lines))).unsafeRunSync()
    println(s"Part 2 result ${res}")
  }


}
