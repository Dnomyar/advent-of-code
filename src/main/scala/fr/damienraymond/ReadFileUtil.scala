package fr.damienraymond

import cats.effect.{IO, Resource}

import scala.io.Source

object ReadFileUtil {

  def readFile(filename: String): Resource[IO, Iterator[String]] =
    Resource.make(IO(Source.fromFile(filename)))(source => IO(source.close()))
      .map(_.getLines())

  def readFileInts(filename: String): Resource[IO, LazyList[Int]] =
    readFile(filename).map(_.to(LazyList).map(_.toInt))

}
