package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.ReadFileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {

  it should "work with the example part1" in {

    Day11.part1("""5483143223
                  |2745854711
                  |5264556173
                  |6141336146
                  |6357385478
                  |4167524645
                  |2176841721
                  |6882881134
                  |4846848554
                  |5283751526""".stripMargin) should be(1656)

  }

  it should "work with the example part2" in {

    Day11.part2("""5483143223
                  |2745854711
                  |5264556173
                  |6141336146
                  |6357385478
                  |4167524645
                  |2176841721
                  |6882881134
                  |4846848554
                  |5283751526""".stripMargin) should contain(195)

  }

  it should "work with this simple example 1" in {
    Day11
      .evolution1(1)(
        Vector(
          Vector(1, 1, 1, 1, 1),
          Vector(1, 9, 9, 9, 1),
          Vector(1, 9, 1, 9, 1),
          Vector(1, 9, 9, 9, 1),
          Vector(1, 1, 1, 1, 1)
        )
      )
      ._1 should be(
      Vector(
        Vector(3, 4, 5, 4, 3),
        Vector(4, 0, 0, 0, 4),
        Vector(5, 0, 0, 0, 5),
        Vector(4, 0, 0, 0, 4),
        Vector(3, 4, 5, 4, 3)
      )
    )
  }

  it should "work with this simple example 2" in {
    Day11
      .evolution1(1)(
        Vector(
          Vector(3, 4, 5, 4, 3),
          Vector(4, 0, 0, 0, 4),
          Vector(5, 0, 0, 0, 5),
          Vector(4, 0, 0, 0, 4),
          Vector(3, 4, 5, 4, 3)
        )
      )
      ._1 should be(
      Vector(
        Vector(4, 5, 6, 5, 4),
        Vector(5, 1, 1, 1, 5),
        Vector(6, 1, 1, 1, 6),
        Vector(5, 1, 1, 1, 5),
        Vector(4, 5, 6, 5, 4)
      )
    )
  }

  it should "rule 1 - increase the number of the octopus by 1" in {
    Day11
      .evolution1(1)(
        Vector(
          Vector(3, 4),
          Vector(4, 0)
        )
      )
      ._1 should be(
      Vector(
        Vector(4, 5),
        Vector(5, 1)
      )
    )
  }

  it should "rule 2 - if energy level >= 9, flash" in {
    Day11.evolution1(1)(
      Vector(
        Vector(3, 4),
        Vector(4, 9)
      )
    ) should be(
      (
        Vector(
          Vector(5, 6),
          Vector(6, 0)
        ),
        1,
        None
      )
    )
  }

  it should "part 1" in {
    val day13 = ReadFileUtil
      .readFileLine("day11.txt")
      .use(line => IO(Day11.part1(line.mkString("\n"))))
      .unsafeRunSync()
    println(s"part1 ${day13}")
  }
  it should "part 2" in {
    val day13 = ReadFileUtil
      .readFileLine("day11.txt")
      .use(line => IO(Day11.part2(line.mkString("\n"))))
      .unsafeRunSync()
    println(s"part2 ${day13}")
  }
}
