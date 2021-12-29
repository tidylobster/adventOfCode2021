package main.day1

import main.Utils

object Day1 extends App with Utils {
  val data = getData("src/resources/Day1.txt").split("\n").map(_.toInt)

  def part1(input: Seq[Int]): Int =
    input.sliding(2).map {
      case w if w.head < w.tail.head => 1
      case _ => 0
    }.sum

  def part2(input: Seq[Int]): Int = {
    part1(input.sliding(3).map(_.sum).toSeq)
  }

  println(part1(data))
  println(part2(data))
}
