package fr.damienraymond

import cats.effect.{IO, Resource}

import scala.io.Source

object ReadFileUtil {

  def readFileLine(filename: String): Resource[IO, LazyList[String]] =
    Resource.make(IO(Source.fromResource(filename)))(source => IO(source.close()))
      .map(_.getLines().to(LazyList))

  def readFileLineInts(filename: String): Resource[IO, LazyList[Int]] =
    readFileLine(filename).map(_.map(_.toInt))

}
