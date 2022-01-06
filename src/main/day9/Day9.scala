package main.day9

import main.Utils

case class Matrix(data: Array[Array[Int]]) {
  val width: Int = data.head.length
  val height: Int = data.length
  def get(point: Point): Int = data(point.y)(point.x)
}

case class Point(x: Int, y: Int) {
  def left: Option[Point] = if (x - 1 >= 0) Some(Point(x - 1, y)) else None
  def up: Option[Point] = if (y - 1 >= 0) Some(Point(x, y - 1)) else None
  def right(matrix: Matrix): Option[Point] = if (x + 1 < matrix.width) Some(Point(x + 1, y)) else None
  def down(matrix: Matrix): Option[Point] = if (y + 1 < matrix.height) Some(Point(x, y + 1)) else None

  def getAdjacentPoints(matrix: Matrix): Seq[Point] =
    Seq(left, up, right(matrix), down(matrix)).filter(_.isDefined).map(_.get)
}

object Day9 extends App with Utils {
  val data: String = getData("src/resources/Day9.txt")
  val matrix: Matrix = Matrix(data.split("\n").map(_.split("").map(_.toInt)))

  private def getLowestPoints(matrix: Matrix): Seq[Point] =
    for {
      i <- matrix.data.indices
      j <- matrix.data(i).indices if isLowestLocally(Point(j, i), matrix)
    } yield Point(j, i)

  private def isLowest(value: Int, others: Seq[Int]): Boolean =
    value == (value +: others).min && value != others.min

  private def isLowestLocally(point: Point, matrix: Matrix): Boolean =
    isLowest(matrix.get(point), point.getAdjacentPoints(matrix).map(matrix.get))

  def part1(matrix: Matrix): Int =
    getLowestPoints(matrix).map(matrix.get).map(_ + 1).sum

  private def getBasinPoints(matrix: Matrix)(point: Point): Seq[Point] = {
    def loop(point: Point, basin: Set[Point]): Set[Point] =
      if (basin.contains(point))
        basin
      else
        point.getAdjacentPoints(matrix).foldLeft(basin ++ Set(point)) {
          case (basin, point) if matrix.get(point) == 9 => basin
          case (basin, point) if basin.contains(point) => basin
          case (basin, point) => loop(point, basin)
        }
      loop(point, Set.empty).toSeq
    }

  def part2(matrix: Matrix): Int = {
    val basins =
      for (point <- getLowestPoints(matrix))
      yield getBasinPoints(matrix)(point).size
    basins.sorted(Ordering[Int].reverse).take(3).product
  }

  println(part1(matrix))
  println(part2(matrix))
}
