package main.day2

import main.Utils
import scala.annotation.tailrec

case class Point(x: Int, y: Int)

object Day2 extends App with Utils {
  val data = getData("src/resources/Day2.txt").split("\n")

  def part1(commands: Seq[String]): Int = {
    @tailrec
    def loop(inputs: Seq[String], acc: Point): Point = inputs match {
      case i :: is =>
        val split = i.split(" ")
        (split(0), split(1)) match {
          case ("forward", x) => loop(is, Point(acc.x + x.toInt, acc.y))
          case ("up", y) => loop(is, Point(acc.x, acc.y - y.toInt))
          case ("down", y) => loop(is, Point(acc.x, acc.y + y.toInt))
        }
      case _ => acc
    }

    val result = loop(commands.toList, Point(0, 0))
    result.x * result.y
  }

  def part2(commands: Seq[String]): Int = {
    @tailrec
    def loop(inputs: Seq[String], acc: Point, aim: Int): Point = inputs match {
      case i :: is =>
        val split = i.split(" ")
        (split(0), split(1)) match {
          case ("forward", x) => loop(is, Point(acc.x + x.toInt, acc.y + x.toInt * aim), aim)
          case ("up", y) => loop(is, acc, aim - y.toInt)
          case ("down", y) => loop(is, acc, aim + y.toInt)
        }
      case _ => acc
    }

    val result = loop(commands.toList, Point(0, 0), 0)
    result.x * result.y
  }

  println(part1(data))
  println(part2(data))
}
