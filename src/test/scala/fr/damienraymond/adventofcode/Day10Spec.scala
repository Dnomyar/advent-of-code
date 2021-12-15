package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {

  it should "work with the example part 1" in {
    Day10.part1("""[({(<(())[]>[[{[]{<()<>>
        |[(()[<>])]({[<{<<[]>>(
        |{([(<{}[<>[]}>{[]{[(<()>
        |(((({<>}<{<{<>}{[]{[]{}
        |[[<[([]))<([[{}[[()]]]
        |[{[{({}]{}}([{[{{{}}([]
        |{<[[]]>}<{[{[{[]{()[[[]
        |[<(<(<(<{}))><([]([]()
        |<{([([[(<>()){}]>(<<{{
        |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\n").toList) should be(
      26397
    )
  }

  it should "find incorrect closing chars - 1" in {
    Day10.findIncorrectClosingChars(
      "{([(<{}[<>[]}>{[]{[(<()>"
    ) should contain('}')
  }

  it should "find incorrect closing chars - 2" in {
    Day10.findIncorrectClosingChars(
      "[[<[([]))<([[{}[[()]]]"
    ) should contain(')')
  }

  it should "return print the result for part1" in {
    val res = Day10.input
      .use(lines => IO(Day10.part1(lines.toList)))
      .unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

  it should "return print the result for part2" in {
    val res = Day10.input
      .use(lines => IO(Day10.part2(lines.toList)))
      .unsafeRunSync()
    println(s"Part 2 result ${res}")
  }

  it should "work with the example part 2" in {
    Day10.part2(
      """[({(<(())[]>[[{[]{<()<>>
                  |[(()[<>])]({[<{<<[]>>(
                  |{([(<{}[<>[]}>{[]{[(<()>
                  |(((({<>}<{<{<>}{[]{[]{}
                  |[[<[([]))<([[{}[[()]]]
                  |[{[{({}]{}}([{[{{{}}([]
                  |{<[[]]>}<{[{[{[]{()[[[]
                  |[<(<(<(<{}))><([]([]()
                  |<{([([[(<>()){}]>(<<{{
                  |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\n").toList
    ) should be(
      288957
    )
  }

}
