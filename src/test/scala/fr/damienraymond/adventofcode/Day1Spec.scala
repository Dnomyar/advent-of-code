package fr.damienraymond.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {

  behavior of "rateOfDepthIncrease"

  it should "compute the right depth for the example input" in {
    Day1.rateOfDepthIncrease(List(199,200,208,210,200,207,240,269,260,263)) should be (7)
  }

  it should "return 0 if the input is empty" in {
    Day1.rateOfDepthIncrease(List.empty) should be (0)
  }

  it should "return 1 with the list 199,200" in {
    Day1.rateOfDepthIncrease(List(199,200)) should be (1)
  }

  it should "show the result" in {
    println(Day1.rateOfDepthIncrease(Day1Input.input))
  }



}
