package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {

  "the example" should "yield 198" in {
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
    Day3.program(input) should be (198)
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
    val res = Day3.day3Input.use(lines => IO(Day3.program(lines))).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }


}
