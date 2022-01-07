package fr.damienraymond.adventofcode

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fr.damienraymond.ReadFileUtil
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {

  it should "part1 one fold" in {
    Day13.part1("""6,10
                  |0,14
                  |9,10
                  |0,3
                  |10,4
                  |4,11
                  |6,0
                  |6,12
                  |4,1
                  |0,13
                  |10,12
                  |3,4
                  |3,0
                  |8,4
                  |1,10
                  |2,14
                  |8,10
                  |9,0
                  |
                  |fold along y=7""".stripMargin) should be(17)
  }

  it should "part1 two fold" in {
    Day13.part1("""6,10
                  |0,14
                  |9,10
                  |0,3
                  |10,4
                  |4,11
                  |6,0
                  |6,12
                  |4,1
                  |0,13
                  |10,12
                  |3,4
                  |3,0
                  |8,4
                  |1,10
                  |2,14
                  |8,10
                  |9,0
                  |
                  |fold along y=7
                  |fold along x=5""".stripMargin) should be(16)
  }

  it should "parse" in {
    Day13.parse("""6,10
                  |0,14
                  |9,10
                  |0,3
                  |10,4
                  |4,11
                  |6,0
                  |6,12
                  |4,1
                  |0,13
                  |10,12
                  |3,4
                  |3,0
                  |8,4
                  |1,10
                  |2,14
                  |8,10
                  |9,0
                  |
                  |fold along y=7
                  |fold along x=5""".stripMargin) should be(
      Day13.TransparentPaper(
        Set(
          (6, 10),
          (0, 14),
          (9, 10),
          (0, 3),
          (10, 4),
          (4, 11),
          (6, 0),
          (6, 12),
          (4, 1),
          (0, 13),
          (10, 12),
          (3, 4),
          (3, 0),
          (8, 4),
          (1, 10),
          (2, 14),
          (8, 10),
          (9, 0)
        ),
        List(
          Right(7),
          Left(5)
        )
      )
    )
  }

  it should "fold on y" in {
    """.
      |#
      |.
      |.
      |.
      |-
      |.
      |.
      |.
      |.
      |#""".stripMargin

    Day13
      .fold(
        Day13.TransparentPaper(
          Set(
            (0, 1),
            (0, 10)
          ),
          List(Right(5))
        )
      )
      .dots should be(
      Set(
        (0, 1),
        (0, 0)
      )
    )
  }

  it should "fold on x" in {
    Day13
      .fold(
        Day13.TransparentPaper(
          Set(
            (1, 0),
            (10, 0)
          ),
          List(Left(5))
        )
      )
      .dots should be(
      Set(
        (1, 0),
        (0, 0)
      )
    )
  }

  it should "part 1" in {
    val day13 = ReadFileUtil
      .readFileLine("day13.txt")
      .use(line => IO(Day13.part1(line.mkString("\n"))))
      .unsafeRunSync()
    println(s"part1 ${day13}")
  }

  it should "part 2" in {
    val day13 = ReadFileUtil
      .readFileLine("day13.txt")
      .use(line => IO(Day13.part2(line.mkString("\n"))))
      .unsafeRunSync()
    println(s"part2 ${day13}")
  }

}
