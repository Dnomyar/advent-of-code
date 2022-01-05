package fr.damienraymond.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {

  val simpleInput =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin

  it should "work with the small example" in {
    Day12.part1(simpleInput) should be(10)
  }

  it should "work with the larger example" in {
    Day12.part1("""fs-end
                  |he-DX
                  |fs-he
                  |start-DX
                  |pj-DX
                  |end-zg
                  |zg-sl
                  |zg-pj
                  |pj-he
                  |RW-he
                  |fs-DX
                  |pj-RW
                  |zg-RW
                  |start-pj
                  |he-WI
                  |zg-he
                  |pj-fs
                  |start-RW""".stripMargin) should be(226)
  }

  it should "work with the full example 1" in {
    val res = Day12.part1("""RT-start
                            |bp-sq
                            |em-bp
                            |end-em
                            |to-MW
                            |to-VK
                            |RT-bp
                            |start-MW
                            |to-hr
                            |sq-AR
                            |RT-hr
                            |bp-to
                            |hr-VK
                            |st-VK
                            |sq-end
                            |MW-sq
                            |to-RT
                            |em-er
                            |bp-hr
                            |MW-em
                            |st-bp
                            |to-start
                            |em-st
                            |st-end
                            |VK-sq
                            |hr-st""".stripMargin)
    println(s"Part1 $res")
  }

  it should "work with the full example 2" in {
    val res = Day12.part2("""RT-start
                            |bp-sq
                            |em-bp
                            |end-em
                            |to-MW
                            |to-VK
                            |RT-bp
                            |start-MW
                            |to-hr
                            |sq-AR
                            |RT-hr
                            |bp-to
                            |hr-VK
                            |st-VK
                            |sq-end
                            |MW-sq
                            |to-RT
                            |em-er
                            |bp-hr
                            |MW-em
                            |st-bp
                            |to-start
                            |em-st
                            |st-end
                            |VK-sq
                            |hr-st""".stripMargin)
    println(s"Part2 $res")
  }

  it should "parse into adjacency list" in {
    Day12.parseIntoAdjacencyList(simpleInput) should be(
      Map(
        "start" -> Set("A", "b"),
        "c" -> Set("A"),
        "A" -> Set("c", "start", "b", "end"),
        "b" -> Set("d", "start", "A", "end"),
        "d" -> Set("b"),
        "end" -> Set("b", "A")
      )
    )
  }

  it should "find the most simple path" in {
    val graph = Map(
      "start" -> Set("end")
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "end")
      )
    )
  }

  it should "find the most simple path with one cave" in {
    val graph = Map(
      "start" -> Set("A"),
      "A" -> Set("end")
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "A", "end")
      )
    )
  }

  it should "find the most simple path with two caves in parallel" in {
    val graph = Map(
      "start" -> Set("A", "B"),
      "B" -> Set("end"),
      "A" -> Set("end")
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "A", "end"),
        List("start", "B", "end")
      )
    )
  }

  it should "ignore dead ends" in {
    val graph: Map[String, Set[String]] = Map(
      "start" -> Set("A"),
      "A" -> Set("end", "b"),
      "b" -> Set()
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "A", "end")
      )
    )
  }

  it should "circulate via small cave only once" in {
    val graph = Map(
      "start" -> Set("A", "b"),
      "b" -> Set("end", "A"),
      "A" -> Set("end", "b")
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "A", "end"),
        List("start", "b", "end"),
        List("start", "A", "b", "A", "end"),
        List("start", "A", "b", "end"),
        List("start", "b", "A", "end")
      )
    )
  }

  it should "find distinct path" in {
    val graph = Map(
      "start" -> Set("A", "b"),
      "c" -> Set("A"),
      "A" -> Set("c", "start", "b", "end"),
      "b" -> Set("d", "start", "A", "end"),
      "d" -> Set("b"),
      "end" -> Set("b", "A")
    )
    Day12.distinctPaths("start", 1)(graph) should be(
      Set(
        List("start", "A", "b", "A", "c", "A", "end"),
        List("start", "A", "b", "A", "end"),
        List("start", "A", "b", "end"),
        List("start", "A", "c", "A", "b", "A", "end"),
        List("start", "A", "c", "A", "b", "end"),
        List("start", "A", "c", "A", "end"),
        List("start", "A", "end"),
        List("start", "b", "A", "c", "A", "end"),
        List("start", "b", "A", "end"),
        List("start", "b", "end")
      )
    )
  }

  it should "allow to go through small cave twice" in {
    val graph = Map(
      "start" -> Set("A", "b"),
      "c" -> Set("A"),
      "A" -> Set("c", "start", "b", "end"),
      "b" -> Set("d", "start", "A", "end"),
      "d" -> Set("b"),
      "end" -> Set("b", "A")
    )
    Day12.distinctPaths("start", 2)(graph) should be(
      Set(
        List("start", "A", "b", "A", "b", "A", "c", "A", "end"),
        List("start", "A", "b", "A", "b", "A", "end"),
        List("start", "A", "b", "A", "b", "end"),
        List("start", "A", "b", "A", "c", "A", "b", "A", "end"),
        List("start", "A", "b", "A", "c", "A", "b", "end"),
        List("start", "A", "b", "A", "c", "A", "c", "A", "end"),
        List("start", "A", "b", "A", "c", "A", "end"),
        List("start", "A", "b", "A", "end"),
        List("start", "A", "b", "d", "b", "A", "c", "A", "end"),
        List("start", "A", "b", "d", "b", "A", "end"),
        List("start", "A", "b", "d", "b", "end"),
        List("start", "A", "b", "end"),
        List("start", "A", "c", "A", "b", "A", "b", "A", "end"),
        List("start", "A", "c", "A", "b", "A", "b", "end"),
        List("start", "A", "c", "A", "b", "A", "c", "A", "end"),
        List("start", "A", "c", "A", "b", "A", "end"),
        List("start", "A", "c", "A", "b", "d", "b", "A", "end"),
        List("start", "A", "c", "A", "b", "d", "b", "end"),
        List("start", "A", "c", "A", "b", "end"),
        List("start", "A", "c", "A", "c", "A", "b", "A", "end"),
        List("start", "A", "c", "A", "c", "A", "b", "end"),
        List("start", "A", "c", "A", "c", "A", "end"),
        List("start", "A", "c", "A", "end"),
        List("start", "A", "end"),
        List("start", "b", "A", "b", "A", "c", "A", "end"),
        List("start", "b", "A", "b", "A", "end"),
        List("start", "b", "A", "b", "end"),
        List("start", "b", "A", "c", "A", "b", "A", "end"),
        List("start", "b", "A", "c", "A", "b", "end"),
        List("start", "b", "A", "c", "A", "c", "A", "end"),
        List("start", "b", "A", "c", "A", "end"),
        List("start", "b", "A", "end"),
        List("start", "b", "d", "b", "A", "c", "A", "end"),
        List("start", "b", "d", "b", "A", "end"),
        List("start", "b", "d", "b", "end"),
        List("start", "b", "end")
      )
    )
  }

  it should "circulate via small cave only twice" in {
    val graph = Map(
      "start" -> Set("A", "b"),
      "b" -> Set("end", "A"),
      "A" -> Set("end", "b")
    )
    Day12.distinctPaths("start", 2)(graph) should be(
      Set(
        List("start", "A", "end"),
        List("start", "b", "end"),
        List("start", "A", "b", "end"),
        List("start", "b", "A", "end"),
        List("start", "b", "A", "b", "end"),
        List("start", "b", "A", "b", "A", "end"),
        List("start", "A", "b", "A", "end"),
        List("start", "A", "b", "A", "b", "end"),
        List("start", "A", "b", "A", "b", "A", "end")
      )
    )
  }

  it should "canGoThroughSmallCave" in {
    Day12.canGoThroughSmallCave(2)(
      List("start", "A", "b", "A", "b", "A", "end"),
      "b"
    ) should be(false)

    Day12.canGoThroughSmallCave(2)(
      List("start", "A", "b", "A", "b", "A", "end"),
      "c"
    ) should be(true)

    Day12.canGoThroughSmallCave(2)(
      List("start", "A", "b", "A", "A", "end"),
      "b"
    ) should be(true)

    Day12.canGoThroughSmallCave(2)(
      List("start", "b", "d", "b", "A"),
      "c"
    ) should be(true)
  }

}
