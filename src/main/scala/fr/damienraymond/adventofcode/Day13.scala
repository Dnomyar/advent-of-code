package fr.damienraymond.adventofcode

object Day13 {

  case class TransparentPaper(
      dots: Set[(Int, Int)],
      folds: List[Either[Int, Int]]
  )

  def part1(input: String): Int =
    (parse _ andThen keepFirstFold andThen fold andThen countDots)(input)

  def part2(input: String): Int =
    (parse _ andThen fold andThen countDots)(input)

  def keepFirstFold(transparentPaper: TransparentPaper): TransparentPaper = {
    transparentPaper.copy(
      folds = transparentPaper.folds.take(1)
    )
  }

  def parse(stripMargin: String): TransparentPaper = {
    val (rawDots, rawFolds) = stripMargin.split("\n").span(_ != "")
    val dots: Set[(Int, Int)] = rawDots.toSet[String].map { case s"$x,$y" =>
      (x.toInt, y.toInt)
    }
    val folds = rawFolds.tail.map {
      case s"fold along y=$y" => Right(y.toInt)
      case s"fold along x=$x" => Left(x.toInt)
    }.toList
    TransparentPaper(
      dots = dots,
      folds = folds
    )
  }

  def fold(transparentPaper: TransparentPaper): TransparentPaper = {
    println(s"FOLDS = ${transparentPaper.folds}")

    val newDots = transparentPaper.folds.scanLeft(transparentPaper.dots) {
      case (dots, Left(x0)) =>
        val (smaller, larger) =
          dots
            .partition(_._1 < x0)

        val maxX = larger.map(_._1).max

        smaller ++ larger.map { case (x, y) =>
          (maxX - x, y)
        }
      case (dots, Right(y0)) =>
        val (smaller, larger) =
          dots
            .partition(_._2 < y0)

//        println(s"smaller = ${smaller}")
//        println(s"larger = ${larger}")

        val maxY = larger.map(_._2).max

        smaller ++ larger.map { case (x, y) =>
          (x, maxY - y)
        }
    }

//    println("Debug")
//    newDots.zip("INIT" :: transparentPaper.folds.map(_.toString)).foreach {
//      case (dots, fold) =>
//        println(s"Fold: $fold")
//        println(debug(transparentPaper.copy(dots = dots)))
//    }

    val res = transparentPaper.copy(
      dots = newDots.last
    )
    println(debug(res))

    res

  }

  def countDots(transparentPaper: TransparentPaper): Int =
    transparentPaper.dots.size

  def debug(transparentPaper: TransparentPaper): String = {
    val xMax = transparentPaper.dots.map(_._1).max
    val yMax = transparentPaper.dots.map(_._2).max
    (0 to yMax)
      .map { y =>
        (0 to xMax).map { x =>
          if (transparentPaper.dots.contains((x, y))) "#"
          else "."
        }.mkString
      }
      .mkString("\n")
  }

}
