package main.day3

import main.Utils
import scala.annotation.tailrec

case class State(state: Seq[Int]) {
  def update(input: String): State =
    State(input.split("").zip(state).map {
      case (x, s) if x == "0" => s - 1
      case (x, s) if x == "1" => s + 1
    })

  lazy val mostCommon: Seq[Int] = state.map(s => if (s >= 0) 1 else 0)
  lazy val leastCommon: Seq[Int] = state.map(s => if (s >= 0) 0 else 1)
}

object Day3 extends App with Utils {
  val data = getData("src/resources/Day3.txt").split("\n").toList

  def scan(reports: Seq[String]): State = {
    @tailrec
    def loop(input: Seq[String], state: State): State = input match {
      case x :: xs => loop(xs, state.update(x))
      case _ => state
    }
    loop(reports, State(Array.fill(reports.head.length)(0)))
  }

  def filter(reports: Seq[String], getCommonBits: Seq[String] => Seq[Int]): String = {
    @tailrec
    def walkOnce(unfiltered: Seq[String], filtered: Seq[String], index: Int, commonBits: Seq[Int]): Seq[String] =
      (unfiltered, filtered) match {
        case (x :: Nil, Nil) => Seq(x)
        case (x :: xs, _) =>
          val targetBit = x.split("")(index).toInt
          if (targetBit == commonBits(index))
            walkOnce(xs, filtered ++ Seq(x), index, commonBits)
          else
            walkOnce(xs, filtered, index, commonBits)
        case (Nil, _) => filtered
      }

    @tailrec
    def walkMany(unfiltered: Seq[String], index: Int): String = {
      val commonBits = getCommonBits(unfiltered)
      walkOnce(unfiltered, Seq.empty, index, commonBits) match {
        case x :: Nil => x
        case Nil => throw new Exception()
        case filtered => walkMany(filtered, index + 1)
      }
    }
    walkMany(reports, 0)
  }

  def convertBits(report: String) = Integer.parseInt(report, 2)

  def part1(reports: Seq[String]): Int = {
    val state = scan(reports)
    convertBits(state.mostCommon.mkString("")) * convertBits(state.leastCommon.mkString(""))
  }

  def part2(reports: Seq[String]): Int = {
    val o2 = filter(reports, {input => scan(input).mostCommon})
    val co2 = filter(reports, {input => scan(input).leastCommon})
    convertBits(o2) * convertBits(co2)
  }

  println(part1(data))
  println(part2(data))
}
