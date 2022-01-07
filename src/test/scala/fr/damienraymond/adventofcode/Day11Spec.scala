package fr.damienraymond.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {

  it should "work with the example" in {

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

  it should "increase a 2x3 matrix by 1" in {
    val matrix =
      Vector(
        Vector(6, 5, 9),
        Vector(3, 8, 5)
      )
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(8, 8, 0),
        Vector(5, 0, 8)
      )
    )
  }

  it should "increase a 3x3 matrix by 1" in {
    val matrix =
      Vector(
        Vector(1, 1),
        Vector(1, 2)
      )
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(2, 2),
        Vector(2, 3)
      )
    )
  }

  it should "increase a 2x2 matrix by 1 - number of flash" in {
    val matrix =
      Vector(
        Vector(1, 1),
        Vector(1, 2)
      )
    Day11.increaseMatrix(matrix)(1)._1 should be(0)
  }

  it should "increase a 2x2 matrix by 1 - overflow to 9 to 0" in {
    val matrix =
      Vector(
        Vector(9, 9),
        Vector(9, 9)
      )
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(0, 0),
        Vector(0, 0)
      )
    )
  }

  it should "increase a 2x2 matrix by 1 - overflow and blip" in {
    val matrix =
      Vector(
        Vector(1, 1),
        Vector(1, 9)
      )
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(3, 3),
        Vector(3, 0)
      )
    )
  }

  it should "increase a 2x2 matrix by 1 - 1 flash" in {
    val matrix =
      Vector(
        Vector(1, 1),
        Vector(1, 9)
      )
    Day11.increaseMatrix(matrix)(1)._1 should be(1)
  }

  it should "increase a 2x2 matrix by 6 - no overflow" in {
    val matrix =
      Vector(
        Vector(3, 3),
        Vector(3, 0)
      )
    Day11.increaseMatrix(matrix)(6)._2 should be(
      Vector(
        Vector(9, 9),
        Vector(9, 6)
      )
    )
  }

  it should "increase a 2x2 matrix by 7 - with overflow" in {
    val matrix =
      Vector(
        Vector(9, 9),
        Vector(9, 6)
      )
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(0, 0),
        Vector(0, 0)
      )
    )
  }

  it should "increase a 2x2 matrix by 7 - 3 flash" in {
    val matrix =
      Vector(
        Vector(9, 9),
        Vector(9, 6)
      )
    Day11.increaseMatrix(matrix)(1)._1 should be(4)
  }

  it should "increase a 2x2 matrix by 7 - 10 flash" in {
    val matrix =
      Vector(
        Vector(1, 1, 1, 1, 1),
        Vector(1, 9, 9, 9, 1),
        Vector(1, 9, 1, 9, 1),
        Vector(1, 9, 9, 9, 1),
        Vector(1, 1, 1, 1, 1)
      )
    Day11.increaseMatrix(matrix)(1)._1 should be(9)
  }

  it should "increase the matrix" in {
    val matrix =
      Vector(
        Vector(1, 1, 1, 1, 1),
        Vector(1, 9, 9, 9, 1),
        Vector(1, 9, 1, 9, 1),
        Vector(1, 9, 9, 9, 1),
        Vector(1, 1, 1, 1, 1)
      )
    println(Day11.printMatrix(matrix))
    println()
    println(Day11.printMatrix(Day11.increaseMatrix(matrix)(1)._2))
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Vector(
        Vector(3, 4, 5, 4, 3),
        Vector(4, 0, 0, 0, 4),
        Vector(5, 0, 0, 0, 5),
        Vector(4, 0, 0, 0, 4),
        Vector(3, 4, 5, 4, 3)
      )
    )
  }

//  it should "countNumberOfFlash - simple 1" in {
//    val matrix =
//      Day11.parse(
//        """11111
//          |19991
//          |19191
//          |19991
//          |11111""".stripMargin
//      )
//    Day11.countNumberOfFlash(matrix)(1) should be(9)
//  }
//
//  it should "countNumberOfFlash - simple 2" in {
//    val matrix =
//      Day11.parse(
//        """11111
//          |19991
//          |19191
//          |19991
//          |11111""".stripMargin
//      )
//    Day11.countNumberOfFlash(matrix)(9) should be(9)
//  }
//
  it should "countNumberOfFlash 1" in {
    val matrix =
      Day11.parse(
        """5483143223
          |2745854711
          |5264556173
          |6141336146
          |6357385478
          |4167524645
          |2176841721
          |6882881134
          |4846848554
          |5283751526""".stripMargin
      )
    Day11.increaseMatrix(matrix)(1)._1 should be(0)
  }

  it should "countNumberOfFlash 2" in {
    val matrix =
      Day11.parse(
        """6594254334
          |3856965822
          |6375667284
          |7252447257
          |7468496589
          |5278635756
          |3287952832
          |7993992245
          |5957959665
          |6394862637""".stripMargin
      )

    println(Day11.printMatrix(Day11.increaseMatrix(matrix)(1)._2))
    Day11.increaseMatrix(matrix)(1)._2 should be(
      Day11.parse(
        """8807476555
          |5089087054
          |8597889608
          |8485769600
          |8700908800
          |6600088989
          |6800005943
          |0000007456
          |9000000876
          |8700006848""".stripMargin
      )
    )
  }

}
