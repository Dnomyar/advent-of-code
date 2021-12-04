package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

import scala.annotation.tailrec

object Day4 {

  trait BingoGrid {
    def calledNumber(number: Int): BingoGrid

    def hasWon: Boolean

    def sumOfUnmarkedNumbers: Int
  }

  case class BingoGridImplementation(lineColumnMatrix: Vector[Vector[Int]],
                                     columnLineMatrix: Vector[Vector[Int]],
                                     calledNumbers: List[Int]) extends BingoGrid {
    override def calledNumber(number: Int): BingoGrid =
      copy(calledNumbers = number :: calledNumbers)

    override def hasWon: Boolean = {
      def lineContains(matrix: Vector[Vector[Int]]): Boolean = {
        matrix.exists { line =>
          line.forall(calledNumbers.contains)
        }
      }

      lineContains(lineColumnMatrix) || lineContains(columnLineMatrix)
    }

    override def sumOfUnmarkedNumbers: Int =
      lineColumnMatrix.flatten.filterNot(calledNumbers.contains).sum
  }

  object BingoGrid {

    def fromString(string: String): Option[BingoGrid] = {

      val lineColumnMatrix = string.split("\n").toVector
        .map(
          _.split(" +")
            .filter(_.nonEmpty)
            .map(_.toInt).toVector
        )
      val columnLineMatrix = lineColumnMatrix.transpose

      Some(BingoGridImplementation(lineColumnMatrix, columnLineMatrix, Nil))
    }
  }

//  def findWinningGrid(grids: Vector[BingoGrid], numberCalled: List[Int]): Option[(BingoGrid, Int)] = {
//    @tailrec
//    def loop(grids: Vector[BingoGrid], numberCalled: List[Int]): Option[(BingoGrid, Int)] = {
//      numberCalled match {
//        case head :: tail =>
//          val updatedGrids = grids.map(_.calledNumber(head))
//          updatedGrids.find(_.hasWon) match {
//            case Some(grid) => Some((grid, head))
//            case None => loop(updatedGrids, tail)
//          }
//        case Nil => None
//      }
//    }
//    loop(grids, numberCalled)
//  }
//

  def parse(input: String): Option[(Vector[BingoGrid], List[Int])] = {
    input.split("\n\n").toList match {
      case headers :: rawGrids =>
        val grids = rawGrids.flatMap(BingoGrid.fromString)
        val calledNumbers = headers.split(",")
        Some((grids.toVector, calledNumbers.toList.map(_.toInt)))
      case _ => None
    }
  }

  def scorePart1(grids: Vector[BingoGrid], numbersCalled: List[Int]): Option[Int] = {

    @tailrec
    def findWinningGrid(grids: Vector[BingoGrid], numberCalled: List[Int]): Option[(BingoGrid, Int)] = {
      numberCalled match {
        case head :: tail =>
          val updatedGrids = grids.map(_.calledNumber(head))
          updatedGrids.find(_.hasWon) match {
            case Some(grid) => Some((grid, head))
            case None => findWinningGrid(updatedGrids, tail)
          }
        case Nil => None
      }
    }

    findWinningGrid(grids, numbersCalled).map{
      case (grid, winningNumber) => grid.sumOfUnmarkedNumbers * winningNumber
    }
  }

  def parseAndProgramPart1(input: String): Option[Int] = {
    parse(input).flatMap{
      case (grids, numbersCalled) => scorePart1(grids, numbersCalled)
    }
  }


  def parseAndProgramPart2(input: String): Option[Int] = {
    parse(input).flatMap{
      case (grids, numbersCalled) => scorePart2(grids, numbersCalled)
    }
  }

  def scorePart2(grids: Vector[BingoGrid], numbersCalled: List[Int]): Option[Int] = {
    @tailrec
    def loop(grids: Vector[BingoGrid], numberCalled: List[Int]): Option[(BingoGrid, Int)] = {
      numberCalled match {
        case head :: tail =>
          val updatedGrids = grids.map(_.calledNumber(head))
          if(updatedGrids.filterNot(_.hasWon).size >= 1) loop(updatedGrids.filterNot(_.hasWon), tail)
          else{
            updatedGrids.find(_.hasWon) match {
              case Some(grid) => Some((grid, head))
              case None => loop(updatedGrids, tail)
            }
          }
        case Nil => None
      }
    }
    loop(grids, numbersCalled).map{
      case (grid, winningNumber) =>
        grid.sumOfUnmarkedNumbers * winningNumber
    }
  }



  val day4Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day4.txt")



}
