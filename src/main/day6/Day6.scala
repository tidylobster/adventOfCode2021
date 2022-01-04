package main.day6

import main.Utils
import scala.collection.mutable

object Day6 extends App with Utils {
  val data = getData("src/resources/Day6.txt").split(",").map(_.toInt).toList
  val cache = mutable.Map.empty[(Int, Int), Long]

  private def computeDescendants(generationsLeft: Int, init: Int): Long = {
    cache.get(generationsLeft, init) match {
      case None =>
        val descendants = for (gens <- generationsLeft - init until 0 by -7) yield computeDescendants(gens, 9)
        val numDescendants = descendants.sum + 1
        cache.update((generationsLeft, init), numDescendants)
        numDescendants
      case Some(numDescendants) => numDescendants
    }
  }

  def part1(input: Seq[Int]): Long =
    input.map(init => computeDescendants(80, init)).sum

  def part2(input: Seq[Int]): Long =
    input.map(init => computeDescendants(256, init)).sum

  println(part1(data))
  println(part2(data))
}
