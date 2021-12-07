package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6Spec extends AnyFlatSpec with Matchers {

  val vector0 = Vector.fill(10)(0)

  it should "generate 26 after 18d for the example given" in {
    Day6.part1("3,4,3,1,2")(18) should be(26)
  }

  it should "generate 5934 after 80d for the example given" in {
    Day6.part1("3,4,3,1,2")(80) should be(5934)
  }
  
  it should "count the number of fish per day" in {
    Day6.countFishPerDay(List(1, 1, 1, 5, 5, 4)) should be(
      vector0.updated(1, 3).updated(5,2).updated(4,1)
    )
  }

  it should "decrease the number days - 1" in {
    Day6.numberOrFishAfter2(1)(vector0.updated(5, 1)) should be(vector0.updated(4, 1))
  }

  it should "decrease the number days - 2" in {
    Day6.numberOrFishAfter2(1)(vector0.updated(5, 1).updated(4, 2)) should be(
      vector0.updated(4, 1).updated(3, 2)
    )
  }

  it should "decrease the number days - 3" in {
    Day6.numberOrFishAfter2(1)(vector0.updated(5,4).updated(4,6).updated(3,8)) should be(
      vector0.updated(4,4).updated(3,6).updated(2,8)
    )
  }

  it should "create a new fish after when it reaches 0" in {
    Day6.numberOrFishAfter2(1)(vector0.updated(0,1)) should be(
      vector0.updated(6,1).updated(8,1)
    )
  }

  it should "repeat the simple operation for 2 days" in {
    Day6.numberOrFishAfter2(2)(vector0.updated(5,2).updated(0,1)) should be(
      vector0.updated(3,2).updated(7,1).updated(5,1)
    )
  }

  it should "keep every element" in {
    Day6.numberOrFishAfter2(1)(vector0.updated(7,1).updated(0,1)) should be(
      vector0.updated(6,2).updated(8,1)
    )
  }

  it should "return print the result for part1" in {
    val res = Day6.day6Input.use(lines =>
      IO(Day6.part1(lines.head)(80))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

}
