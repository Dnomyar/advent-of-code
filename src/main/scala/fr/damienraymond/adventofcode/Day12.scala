package fr.damienraymond.adventofcode

object Day12 {
  def part1(stripMargin: String): Int =
    (parseIntoAdjacencyList _ andThen distinctPaths("start", 1) andThen (
      (paths: Set[List[String]]) => paths.size
    ))(stripMargin)

  def part2(stripMargin: String): Int =
    (parseIntoAdjacencyList _ andThen distinctPaths("start", 2) andThen (
      (paths: Set[List[String]]) => paths.size
    ))(stripMargin)

  def parseIntoAdjacencyList(input: String): Map[String, Set[String]] = {
    val directedGraph: Array[(String, String)] = input
      .split("\n")
      .map(_.split("-"))
      .collect { case Array(source, target) =>
        source -> target
      }
    (directedGraph ++ directedGraph.map(_.swap))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
  }

  def distinctPaths(
      start: String,
      numberOfTimePerSmallCave: Int
  )(graph: Map[String, Set[String]]): Set[List[String]] = {

    def loop(paths: Set[List[String]]): Set[List[String]] = {

      val newPaths = paths.flatMap {
        case path @ ("end" :: _) => Set(path)
        case path @ (lastNode :: _) =>
          graph(lastNode)
            .collect {
              case "end"                               => "end" :: path
              case "start"                             => path
              case neighbours if isBigCave(neighbours) => neighbours :: path
              case neighbours
                  if canGoThroughSmallCave(numberOfTimePerSmallCave)(
                    path,
                    neighbours
                  ) =>
                neighbours :: path
            }
      }

      if (newPaths == paths) paths
      else loop(newPaths)
    }
    loop(Set(List(start)))
      .collect { case path @ ("end" :: _) =>
        path
      }
      .map(_.reverse)
  }

  def canGoThroughSmallCave(
      numberOfTimePerSmallCave: Int
  )(path: List[String], neighbours: String): Boolean = {
    val set = (neighbours :: path)
      .filterNot(isBigCave)
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .values
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap

    if (numberOfTimePerSmallCave == 1)
      set.keys.forall(_ <= 1)
    else
      set.keys.forall(_ <= 2) && set.get(2).forall(_ <= 1)
  }

  private def isBigCave(neighbours: String) = neighbours.forall(_.isUpper)

}
