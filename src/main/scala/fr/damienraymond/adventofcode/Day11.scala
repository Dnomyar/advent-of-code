package fr.damienraymond.adventofcode

object Day11 {
  def part1(input: String): Int = ???

  def parse(input: String): Vector[Vector[Int]] =
    input.split("\n").map(_.split("").map(_.toInt).toVector).toVector

  def neighbours(
      matrix: Vector[Vector[Int]]
  )(i: Int, j: Int): List[(Int, Int)] = {
    List(
      (i - 1, j),
      (i + 1, j),
      (i, j - 1),
      (i, j + 1),
      (i - 1, j - 1),
      (i + 1, j + 1),
      (i + 1, j - 1),
      (i - 1, j + 1)
    ).filter {
      case (i, j)
          if i >= 0 && i < matrix.length && j >= 0 && j < matrix(0).length =>
        true
      case _ => false
    }
  }

  def increaseMatrix(
      matrix: Vector[Vector[Int]]
  )(iteration: Int): (Int, Vector[Vector[Int]]) = {

    val mapMatrix = matrixToMap(matrix)

    val newMatrix =
      (0 until iteration).foldLeft((0, mapMatrix)) {
        case ((numberOf, mapMatrix), _) =>
          val (numberOf2, res) = increaseMatrixByOne(matrix)(mapMatrix)
          (numberOf + numberOf2, res)
      }

    (newMatrix._1, mapToMatrix(newMatrix._2)(matrix))

  }

  private def mapToMatrix(
      newMatrix: Map[(Int, Int), Int]
  )(matrix: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    matrix.indices
      .map(i =>
        matrix(i).indices.map { j =>
          newMatrix(i, j)
        }.toVector
      )
      .toVector
  }

  private def matrixToMap(matrix: Vector[Vector[Int]]): Map[(Int, Int), Int] = {
    matrix.indices
      .flatMap(i =>
        matrix(i).indices.map { j =>
          (i, j) -> matrix(i)(j)
        }
      )
      .toMap
  }

  private def increaseMatrixByOne(
      matrix: Vector[Vector[Int]]
  )(mapMatrix: Map[(Int, Int), Int]): (Int, Map[(Int, Int), Int]) = {

    mapMatrix.filter(_._2 == 0)

    def loop(
        mapMatrix: Map[(Int, Int), Int],
        toVisit: List[(Int, Int)],
        numberOfIteration: Int
    ) = {
      val (updatedMatrix, newToVisit) =
        toVisit.foldLeft((mapMatrix, List.empty[(Int, Int)])) {
          case ((m, coords), coord @ (i, j)) =>
            val nei = neighbours(matrix)(i, j)
            val withNeibourgsupdated =
              nei.foldLeft(m) { case (map, (i1, j1)) =>
                map.updated((i1, j1), map((i1, j1)) + 1)
              }

            (withNeibourgsupdated, coords ++ nei)
        }
      ???
    }

    mapMatrix
      .foldLeft((0, mapMatrix)) {
        case ((numberOfIteration, matrix2), (key @ (i, j), a)) =>
          val res = (a + 1) % 10
          val neighboursBlips =
            neighbours(matrix)(i, j).map(mapMatrix).count(_ == 9)

//          println(
////            s"$key $a -> $res - $neighboursBlips - ${neighbours(matrix)(i, j).map(mapMatrix)}"
//            s"$key $a -> $res - ${(a + 1 + neighboursBlips) / 10}"
//          )
          val i1 = (a + 1 + neighboursBlips) / 10
//            if ((res + neighboursBlips) / 10 > 0) 1
//            else 0

          val value: Int =
            if (res == 0) res
            else ((res + neighboursBlips) % 10)
          (numberOfIteration + i1, matrix2.updated(key, value))
      }

//    mapMatrix.map { case (key @ (i, j), a) =>
//      val res = (a + 1) % 10
//      val neighboursBlips =
//        neighbours(matrix)(i, j).map(mapMatrix).count(_ == 9)
//
////      println(
////        s"$key $a -> $res - $neighboursBlips - ${neighbours(matrix)(i, j).map(mapMatrix)}"
////      )
//      if (res == 0) key -> res
//      else key -> ((res + neighboursBlips) % 10)
//    }
  }

//  def countNumberOfFlash(matrix: Vector[Vector[Int]])(iteration: Int): Int = {
//
//    val matrixAfterIteration = matrix.map(_.map(a => (a + iteration)))
//
//    val afterMatrix = matrix.indices
//      .map(i =>
//        matrix(i).indices.map { j =>
//          //          (matrixAfterIteration(i)(j)) / 10
//
//          val numberOfBlipsAround = neighbours(
//            matrixAfterIteration
//          )(i, j)
//            .map { case (i, j) =>
//              matrixAfterIteration(i)(j)
//            }
//            .toSet[Int]
//            .map(a => a / 10)
//            .sum
//
//          val i1 = matrixAfterIteration(i)(j) + numberOfBlipsAround
//          (i1 % 10, i1 / 10)
//        }
//      )
//
//    println()
//    println(printMatrix(afterMatrix))
//
//    afterMatrix.flatten.map(_._2).sum
//
//  }

  def printMatrix(afterMatrix: Vector[Vector[Int]]) = {
    afterMatrix.map(_.mkString).mkString("\n")
  }
}
