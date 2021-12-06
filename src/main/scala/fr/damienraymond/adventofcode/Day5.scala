package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

import scala.collection.immutable.MultiSet

object Day5 {

  case class Coordinates(x: Int, y: Int)

  sealed trait Line {
    def allPoints: LazyList[Coordinates]
  }

  case class HorizontalLine(y: Int, x0: Int, x1: Int) extends Line {
    override lazy val allPoints: LazyList[Coordinates] = {
      val xMin = Math.min(x0, x1)
      val xMax = Math.max(x0, x1)
      (xMin to xMax).to(LazyList).map(x => Coordinates(x, y))
    }
  }

  case class VerticalLine(x: Int, y0: Int, y1: Int) extends Line {
    override lazy val allPoints: LazyList[Coordinates] = {
      val yMin = Math.min(y0, y1)
      val yMax = Math.max(y0, y1)
      (yMin to yMax).to(LazyList).map(y => Coordinates(x, y))
    }
  }

  case class Diagonal45Line(c0: Coordinates, c1: Coordinates) extends Line {
    override lazy val allPoints: LazyList[Coordinates] = {
      val byX = if(c1.x < c0.x) -1 else 1
      val byY = if(c1.y < c0.y) -1 else 1

      (c0.x to c1.x by byX).zip(c0.y to c1.y by byY).map {
        case (x, y) => Coordinates(x, y)
      }
    }.to(LazyList)

  }

  object Line {
    def apply(c0: Coordinates, c1: Coordinates): Option[Line] = {
      if (c0.x == c1.x)
        Some(VerticalLine(c0.x, c0.y, c1.y))
      else if (c0.y == c1.y)
        Some(HorizontalLine(c0.y, c0.x, c1.x))
      else if (Math.abs(c0.x - c1.x) == Math.abs(c0.y - c1.y))
        Some(Diagonal45Line(c0, c1))
      else None
    }
  }

  def parse(input: LazyList[String]): LazyList[Line] = {
    object int {
      def unapply(str: String): Option[Int] = str.toIntOption
    }
    input.flatMap {
      case s"${int(x0)},${int(y0)} -> ${int(x1)},${int(y1)}" =>
        Line(Coordinates(x0, y0), Coordinates(x1, y1))
    }
  }

  def countIntersection(list: LazyList[Line]): Int = {
    val pointSet = list.flatMap(_.allPoints).foldLeft[MultiSet[Coordinates]](MultiSet.empty[Coordinates])(_.incl(_))
    val intersections = pointSet.filterOccurrences {
      case (_, occurrences) if occurrences > 1 => true
      case _ => false
    }.occurrences
    intersections.size
  }

  def filterStraitLine(input: LazyList[Line]): LazyList[Line] = input.filter {
    case _: HorizontalLine => true
    case _: VerticalLine => true
    case _ => false
  }


  def part1(input: LazyList[String]): Int =
    (parse _ andThen filterStraitLine andThen countIntersection) (input)
  def part2(input: LazyList[String]): Int = (parse _ andThen countIntersection) (input)


  val day5Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day5.txt")


}
