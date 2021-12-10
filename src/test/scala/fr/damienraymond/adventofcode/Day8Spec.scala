package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day8Spec extends AnyFlatSpec with Matchers {

  val input =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin

  it should "count the number of simple numbers" in {
    Day8.part1(input) should be (26)
  }

  it should "parse" in {
    Day8.parse("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe") should be (
      Vector(
        (
          Vector("be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"),
          Vector("fdgacbe","cefdb","cefbgd","gcbe")
        )
      )
    )
  }

  /*
    0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

   */



  it should "return print the result for part1" in {
    val res = Day8.input.use(lines =>
      IO(Day8.part1(lines.mkString("\n")))
    ).unsafeRunSync()
    println(s"Part 1 result ${res}")
  }

  it should "return print the result for part2" in {
    val res = Day8.input.use(lines =>
      IO(Day8.part2(lines.mkString("\n")))
    ).unsafeRunSync()
    println(s"Part 2 result ${res}")
  }


  val line: Day8.Line = Day8.Line(Vector(
    "be", // 1
    "cfbegad", // 8
    "cbdgef", // 0
    "fgaecd",
    "cgeb", // 4
    "fdcge",
    "agebfd",
    "fecdb",
    "fabcd",
    "edb" // 7
  ))
  it should "find 1" in {
    line.`1` should be ("be")
  }
  it should "find 7" in {
    line.`7` should be ("edb")
  }
  it should "find 4" in {
    line.`4` should be ("cgeb")
  }
  it should "find 8" in {
    line.`8` should be ("cfbegad")
  }
  it should "find a" in {
    line.a should be ("d")
  }
  it should "find 0" in {
    line.`0` should be ("agebfd")
  }
  it should "find 9" in {
    line.`9` should be ("cbdgef")
  }
  it should "find 6" in {
    line.`6` should be ("fgaecd")
  }
  it should "find d" in {
    line.d should be ("c")
  }
  it should "find c" in {
    line.c should be ("b")
  }
  it should "find e" in {
    line.e should be ("a")
  }
  it should "find b" in {
    line.b should be ("g")
  }
  it should "find 2" in {
    line.`2` should be ("fabcd")
  }
  it should "find 5" in {
    line.`5` should be ("fdcge")
  }
  it should "find f" in {
    line.f should be ("e")
  }
  it should "find g" in {
    line.g should be ("f")
  }

  it should "recognise the second part" in {
    Day8.countAllNumbers(
      Vector(
        (
          Vector("be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"),
          Vector("fdgacbe","cefdb","cefbgd","gcbe")
        )
      )
    ) should be (8394)
  }

}
