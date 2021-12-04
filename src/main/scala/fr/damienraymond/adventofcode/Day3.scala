package fr.damienraymond.adventofcode

import cats.effect.{IO, Resource}
import fr.damienraymond.ReadFileUtil

object Day3 {

  def programPart1(list: LazyList[String]): Int = {
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
      results.map {
        case (count0, count1) if count0 > count1 => '0'
        case (_, _) => '1'
      }.mkString
    }

    lazy val lessCommonBits: String = {
      results.map {
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


  def oxygen(list: LazyList[String]): LazyList[String] = {
    val oxygenMax: (Int, Int) => Char = (numberOf0: Int, numberOf1: Int) =>
      if (numberOf1 >= numberOf0) '1' else '0'
    filterInDepthBy(list, oxygenMax)
  }
  def co2(list: LazyList[String]): LazyList[String] = {
    val co2Max: (Int, Int) => Char = (numberOf0: Int, numberOf1: Int) =>
      if (numberOf0 <= numberOf1) '0' else '1'
    filterInDepthBy(list, co2Max)
  }

  def filterInDepthBy(list: LazyList[String], getBitToSelect: (Int, Int) => Char): LazyList[String] = {
    val size = list.head.length

    def loop(list: LazyList[String], i: Int, prefix: String): LazyList[String] = {
      if (i < size && list.size > 1) {
        val bits = list.map(_ (i))
        val numberOf0 = bits.count(_ == '0')
        val numberOf1 = bits.count(_ == '1')
        val bitToSelect = getBitToSelect(numberOf0, numberOf1)
        val filtered = list.filter(_ (i) == bitToSelect)
        loop(filtered, i + 1, prefix + bitToSelect)
      }
      else list
    }

    loop(list, 0, "")
  }

  def programPart2(list: LazyList[String]): Int = {
    val oxygenDigit = Integer.parseInt(oxygen(list).head, 2)
    val co2Digit = Integer.parseInt(co2(list).head, 2)
    co2Digit * oxygenDigit
  }


}
