package main.day7

import main.Utils

import scala.annotation.tailrec

case class Index(diff: Int, index: Int)

object Day7 extends App with Utils {
  val data = getData("src/resources/Day7.txt").split(",").map(_.toInt).toList

  def part1(input: Seq[Int]): Int = {
    val inputSorted = input.sorted
    val median = inputSorted(input.length / 2)
    input.map(init => (init - median).abs).sum
  }

  def computeFuelConsumption(input: Seq[Int])(position: Int): Int =
      input.flatMap(init => (init - position).abs to 0 by -1).sum

  def part2(input: Seq[Int]): Int =
    input.indices.map(computeFuelConsumption(input)).min

  println(part1(data))
  println(part2(data))
}
