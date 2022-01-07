package fr.damienraymond.adventofcode

object Day13 {

  case class TransparentPaper(dots: Set[(Int, Int)], folds: Either[Int, Int])

  def part1(input: String): Int =
    (parse _ andThen fold andThen countDots)(input)

  def parse(stripMargin: String): TransparentPaper = {
    val (rawDots, rawFolds) = stripMargin.split("\n").span(_ != "")
    val dots: Set[(Int, Int)] = rawDots.toSet[String].map { case a @ s"$x,$y" =>
      (x.toInt, y.toInt)
    }
    val folds = rawFolds.tail.head match {
      case s"fold along y=$y" => Right(y.toInt)
      case s"fold along x=$x" => Left(x.toInt)
    }
    TransparentPaper(
      dots = dots,
      folds = folds
    )
  }

  def fold(transparentPaper: TransparentPaper): TransparentPaper = {
    println(debug(transparentPaper))

    val newDots = transparentPaper.folds match {
      case Left(x0) =>
        val (smaller, larger) =
          transparentPaper.dots
            .partition(_._1 < x0)

        smaller ++ larger.map { case (x, y) =>
          (x - x0, y)
        }
      case Right(y0) =>
        val (smaller, larger) =
          transparentPaper.dots
            .partition(_._2 < y0)

        println(s"smaller = ${smaller}")
        println(s"larger = ${larger}")

        val maxY = larger.map(_._2).max

        smaller ++ larger.map { case (x, y) =>
          (x, maxY - y)
        }
    }

    val res = transparentPaper.copy(
      dots = newDots
    )
    println(debug(res))

    res

  }

  def countDots(transparentPaper: TransparentPaper): Int =
    transparentPaper.dots.size

  def debug(transparentPaper: TransparentPaper): String = {
    val xMax = transparentPaper.dots.map(_._1).max
    val yMax = transparentPaper.dots.map(_._2).max

    (1 to xMax)
      .map { x =>
        (1 to yMax).map { y =>
          if (transparentPaper.dots.contains((x, y))) "#"
          else "."
        }.mkString
      }
      .mkString("\n")
  }

}
