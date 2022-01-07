package fr.damienraymond.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {

  it should "part1" in {
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
                  |fold along y=7""".stripMargin) should be(
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
        Right(7)
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
          Right(5)
        )
      )
      .dots should be(
      Set(
        (0, 1),
        (0, 0)
      )
    )

  }

}
