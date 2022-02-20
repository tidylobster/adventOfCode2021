package main.day14

import scala.collection.mutable
import main.Utils

object Day14 extends App with Utils {
  val data = getData("src/resources/Day14.txt").split("\n").toList

  val template: String = data.head
  val rules: Map[String, String] = data
    .drop(2)
    .map(_.split(" -> ").toList)
    .map {
      case x :: y :: Nil => x -> y
    }.toMap

  private def polymerize(template: String, rules: Map[String, String]): String =
    template.sliding(2).foldLeft(template.head.toString) {
      case (acc, pair) =>
        val elem = rules(pair)
        acc + elem + pair.last
    }

  private def score(template: String): Long = {
    val counter = mutable.Map[Char, Long]()
    template.foreach(c => counter.update(c, counter.getOrElse(c, 0L) + 1L))
    val mostCommon = counter.maxBy {
      case (_, value) => value
    }
    val leastCommon = counter.minBy {
      case (_, value) => value
    }
    mostCommon._2 - leastCommon._2
  }

  def part1(template: String, rules: Map[String, String]): Long = {
    val result = (1 to 10).foldLeft(template) {
      case (tpl, step) =>
        println(s"Step: $step")
        polymerize(tpl, rules)
    }
    score(result)
  }

  def part2(template: String, rules: Map[String, String]): Long = {
    val result = (1 to 40).foldLeft(template) {
      case (tpl, step) =>
        println(s"Step: $step")
        ???
    }
    score(result)
  }

  println(part1(template, rules))
//  println(part2(template, rules))
}
