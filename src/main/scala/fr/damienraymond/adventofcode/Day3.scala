package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day3 {

  def program(list: LazyList[String]): Int = {
    val counter = countBits(list)
    val lessCommonBits = Integer.parseInt(counter.lessCommonBits, 2)
    val mostCommonBits = Integer.parseInt(counter.mostCommonBits, 2)
    lessCommonBits * mostCommonBits
  }

  def countBits(list: LazyList[String]): Counter = {
    val size = list.head.length
    list.foldLeft(Counter.empty(size))(_.count(_))
  }
  case class Counter(results: Vector[(Int, Int)]) {

    private def update(c: Char, res: (Int, Int)): (Int, Int) = {
      val (count0, count1) = res
      c match {
        case '0' => (count0 + 1, count1)
        case '1' => (count0, count1 + 1)
      }
    }

    def count(string: String): Counter = {
      copy(
        results =
          string.zip(results).map {
            case (c, res) => update(c, res)
          }.toVector
      )
    }

    lazy val mostCommonBits: String = {
      results.map{
        case (count0, count1) if count0 > count1 => '0'
        case (_, _) => '1'
      }.mkString
    }

    lazy val lessCommonBits: String = {
      results.map{
        case (count0, count1) if count0 < count1 => '0'
        case (_, _) => '1'
      }.mkString
    }

  }

  object Counter {
    def empty(size: Int): Counter = Counter(Vector.fill(size)((0, 0)))
  }

  val day3Input: Resource[IO, LazyList[String]] =
    ReadFileUtil.readFileLine("day3.txt")


}
