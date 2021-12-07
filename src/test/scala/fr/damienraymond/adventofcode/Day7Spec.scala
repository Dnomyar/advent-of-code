package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7Spec extends AnyFlatSpec with Matchers {

  it should "work with the example" in {
    Day7.part1("16,1,2,0,4,2,7,1,2,14") should be (37)
  }

  it should "return 37" in {
    Day7.findTheLowestCostOfFuelPart1(Vector(16,1,2,0,4,2,7,1,2,14)) should be (37)
  }


  it should "return print the result for part1" in {
    val res = Day7.day7Input.use(lines =>
      IO(Day7.part1(lines.head))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }


  it should "return 168" in {
    Day7.findTheLowestCostOfFuelPart2(Vector(16,1,2,0,4,2,7,1,2,14)) should be (168)
  }



  it should "return print the result for part2" in {
    val res = Day7.day7Input.use(lines =>
      IO(Day7.part2(lines.head))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }





}
