package main.day11

import main.Utils

import scala.annotation.tailrec

case class Matrix(data: Array[Array[Int]]) {
  val width: Int = data.head.length
  val height: Int = data.length

  def getPoint(x: Int, y: Int): Point = Point(x, y)
  def getValue(point: Point): Int = getValue(point.x, point.y)
  def getValue(x: Int, y: Int): Int = data(y)(x)

  private def set(point: Point, value: Int): Matrix =
    set(point.x, point.y, value)

  private def set(x: Int, y: Int, value: Int): Matrix = {
    val row = data(y).clone()
    val matrix = data.clone()
    row.update(x, value)
    matrix.update(y, row)
    Matrix(matrix)
  }

  def reset(point: Point): Matrix =
    set(point.x, point.y, 0)

  def increment(point: Point): Matrix = point
    .getAdjacent(this)
    .foldLeft(this) {
      case (matrix, point) => matrix.set(point, matrix.getValue(point) + 1)
    }

  def increment: Matrix = Matrix(data.map(_.map(_ + 1)))

  override def toString: String =
    data.map(_.mkString("")).mkString("\n")
}

case class Point(x: Int, y: Int) {
  private val left: Matrix => Option[Point] = (matrix: Matrix) =>
    if (x - 1 >= 0) Some(matrix.getPoint(x - 1, y)) else None
  private val upLeft: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y - 1 >= 0 && x - 1 >= 0) Some(matrix.getPoint(x - 1, y - 1)) else None
  private val up: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y - 1 >= 0) Some(matrix.getPoint(x, y - 1)) else None
  private val upRight: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y - 1 >= 0 && x + 1 < matrix.width) Some(matrix.getPoint(x + 1, y - 1)) else None
  private val right: Matrix => Option[Point] = (matrix: Matrix) =>
    if (x + 1 < matrix.width) Some(matrix.getPoint(x + 1, y)) else None
  private val downRight: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y + 1 < matrix.height && x + 1 < matrix.width) Some(matrix.getPoint(x + 1, y + 1)) else None
  private val down: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y + 1 < matrix.height) Some(matrix.getPoint(x, y + 1)) else None
  private val downLeft: Matrix => Option[Point] = (matrix: Matrix) =>
    if (y + 1 < matrix.height && x - 1 >= 0) Some(matrix.getPoint(x - 1, y + 1)) else None

  def getAdjacent(matrix: Matrix): Seq[Point] = {
    Seq(left, upLeft, up, upRight, right, downRight, down, downLeft)
      .map(_(matrix))
      .filter(_.isDefined)
      .map(_.get)
  }

}

object Day11 extends App with Utils {
  val data: Matrix  = Matrix(getData("src/resources/Day11.txt").split("\n").map(_.split("").map(_.toInt)))

  private def getCharged(matrix: Matrix): Seq[Point] =
    for {
      i <- matrix.data.indices
      j <- matrix.data(i).indices
      value = matrix.getValue(j, i) if value > 9
    } yield matrix.getPoint(j, i)

  private def flash(matrix: Matrix): Matrix = {
    @tailrec
    def loop(matrix: Matrix, flashed: Set[Point]): Matrix =
      getCharged(matrix).toSet.diff(flashed) match {
        case charged if charged.nonEmpty =>
          val updated = charged.foldLeft(matrix) { case (matrix, point) => matrix.increment(point) }
          loop(updated, flashed ++ charged)
        case _ => flashed.foldLeft(matrix) { case (matrix, point) => matrix.reset(point) }
      }
    loop(matrix, Set.empty)
  }

  private def countFlashes(matrix: Matrix): Int =
    matrix.data.map(_.count(_ == 0)).sum

  def part1(matrix: Matrix, steps: Int): Int = {
    var numFlashes: Int = 0
    (1 to steps).foldLeft(matrix) { case (matrix, _) =>
      val result = flash(matrix.increment)
      numFlashes += countFlashes(result)
      result
    }
    numFlashes
  }

  def part2(matrix: Matrix): Int = {
    var result = matrix
    val all = matrix.width * matrix.height
    Stream.from(1).dropWhile { _ =>
      result = flash(result.increment)
      countFlashes(result) != all
    }.head
  }

  println(part1(data, 100))
  println(part2(data))
}
