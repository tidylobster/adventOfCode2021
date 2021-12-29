package main.day5

import main.Utils

import scala.annotation.tailrec

case class Point(x: Int, y: Int)
object Point {
  def from(input: String): Point = {
    val crd = input.split(",").map(_.toInt)
    Point(crd(0), crd(1))
  }
}

case class Line(p1: Point, p2: Point) {
  def points: Seq[Point] = (p1, p2) match {
    case (Point(x1, y1), Point(x2, y2)) if x1 == x2 => (y1.min(y2) to y1.max(y2)).map(Point(x1, _))
    case (Point(x1, y1), Point(x2, y2)) if y1 == y2 => (x1.min(x2) to x1.max(x2)).map(Point(_, y1))
    case (Point(x1, y1), Point(x2, y2)) if (x1 max x2) - (x1 min x2) == (y1 max y2) - (y1 min y2) => // part 2
      if (x1 < x2 && y1 < y2)
        (0 to x2 - x1).map(i => Point(x1 + i, y1 + i))
      else if (x1 < x2 && y1 > y2)
        (0 to x2 - x1).map(i => Point(x1 + i, y1 - i))
      else if (x1 > x2 && y1 < y2)
        (0 to x1 - x2).map(i => Point(x1 - i, y1 + i))
      else
        (0 to x1 - x2).map(i => Point(x1 - i, y1 - i))
    case _ => Seq.empty
  }
}
object Line {
  def from(input: String): Line = {
    val crds = input.split(" -> ")
    Line(Point.from(crds(0)), Point.from(crds(1)))
  }
}

object Day5 extends App with Utils {
  val data = getData("src/resources/Day5.txt").split("\n").toList
  val lines: Seq[Line] = data.map(Line.from)

  def part1(lines: Seq[Line]) = {
    @tailrec
    def loop(points: Seq[Point], counter: Map[Point, Int]): Map[Point, Int] = points match {
      case x :: xs => loop(xs, counter.updated(x, counter.getOrElse(x, 0) + 1))
      case Nil => counter
    }
    val points = loop(lines.flatMap(_.points), Map.empty)
    points.count { case (_, c) => c >= 2 }
  }

  println(part1(lines))
}
