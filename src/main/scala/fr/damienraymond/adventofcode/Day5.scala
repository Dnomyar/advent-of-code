package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

import scala.collection.immutable.MultiSet

object Day5 {

  case class Coordinates(x: Int, y: Int)

  sealed trait StraitLine {
    def allPoints: LazyList[Coordinates]
  }

  case class HorizontalLine(y: Int, x0: Int, x1: Int) extends StraitLine {
    override def allPoints: LazyList[Coordinates] =
      (x0 to x1).to(LazyList).map(x => Coordinates(x, y))
  }
  case class VerticalLine(x: Int, y0: Int, y1: Int) extends StraitLine {
    override def allPoints: LazyList[Coordinates] =
      (y0 to y1).to(LazyList).map(y => Coordinates(x, y))
  }

  object StraitLine {
    def apply(c0: Coordinates, c1: Coordinates): Option[StraitLine] = {
      if(c0.x == c1.x)
        Some(VerticalLine(c0.x, Math.min(c0.y, c1.y), Math.max(c0.y, c1.y)))
      else if(c0.y == c1.y)
        Some(HorizontalLine(c0.y, Math.min(c0.x, c1.x), Math.max(c0.x, c1.x)))
      else None
    }
  }

  def part1(input: LazyList[String]): Int = (parse _ andThen countIntersection)(input)

  def parse(input: LazyList[String]): LazyList[StraitLine] = {
    object int {
      def unapply(str: String): Option[Int] = str.toIntOption
    }
    input.flatMap{
      case s"${int(x0)},${int(y0)} -> ${int(x1)},${int(y1)}" =>
        StraitLine(Coordinates(x0,y0), Coordinates(x1,y1))
    }
  }

  def countIntersection(list: LazyList[StraitLine]): Int = {
    val pointSet = list.flatMap(_.allPoints).foldLeft[MultiSet[Coordinates]](MultiSet.empty[Coordinates])(_.incl(_))

    val intersections = pointSet.filterOccurrences {
      case (_, occurrences) if occurrences > 1 => true
      case _ => false
    }.occurrences
    intersections.size
  }



  val day5Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day5.txt")


}
