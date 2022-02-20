package main.day13

import main.Utils

sealed trait Fold
case class Up(value: Int) extends Fold
case class Left(value: Int) extends Fold

object Day13 extends App with Utils {
  type Coordinate = (Int, Int)
  type Matrix = Array[Array[Int]]

  val data: Seq[String] = getData("src/resources/Day13.txt").split("\n").toList

  val points: Seq[Coordinate] = data
    .takeWhile(_.nonEmpty)
    .map(_.split(",").map(_.toInt).toList)
    .map {
      case x :: y :: Nil => x -> y
    }

  val instructions: Seq[Fold] = data
    .dropWhile(_.nonEmpty)
    .tail
    .map(_.split(" ").last)
    .map {
      case y if y.startsWith("y") => Up(y.drop(2).toInt)
      case x => Left(x.drop(2).toInt)
    }

  private def getMatrix(points: Seq[Coordinate]): Matrix = {
    val maxWidth = points.map(_._1).max
    val maxHeight = points.map(_._2).max
    val matrix = Array.fill(maxHeight + 1)(Array.fill(maxWidth + 1)(0))
    points.foreach { case (x, y) => matrix(y).update(x, 1) }
    matrix
  }

  private def fold(matrix: Matrix, fold: Fold): Matrix = fold match {
    case Up(value) =>
      val (preUp, preBottom) = matrix.splitAt(value)
      val (up, bottom) = (preUp, preBottom.tail) match {
        case (u, b) if u.length > b.length =>
          val stub = Array.fill(u.length - b.length, u.head.length)(0)
          (u, b ++ stub)
        case (u, b) if u.length < b.length =>
          val stub = Array.fill(b.length - u.length, b.head.length)(0)
          (stub ++ u, b)
        case x => x
      }
      up
        .zip(bottom.reverse)
        .map {
          case (ur, br) => ur
            .zip(br)
            .map {
              case (x, y) => x max y
            }
        }
    case Left(value) =>
      matrix.map(row => {
        val (preLeft, preRight) = row.splitAt(value)
        val (left, right) = (preLeft, preRight.tail) match {
          case (l, r) if l.length > r.length =>
            val stub = Array.fill(l.length - r.length)(0)
            (l, r ++ stub)
          case (l, r) if l.length < r.length =>
            val stub = Array.fill(r.length - l.length)(0)
            (stub ++ l, r)
          case x => x
        }
        left
          .zip(right.reverse)
          .map { case (x, y) => x max y }
      })
  }

  private def show(matrix: Matrix): Unit =
    println(matrix.map(_.map(x => if (x == 1) "█" else " ").mkString("")).mkString("\n"))

  def part1(points: Seq[Coordinate], instructions: Seq[Fold]): Int = {
    val matrix = getMatrix(points)
    val folded = fold(matrix, instructions.head)
    folded.map(_.sum).sum
  }

  def part2(points: Seq[Coordinate], instructions: Seq[Fold]): Unit = {
    val matrix = getMatrix(points)
    val folded = instructions.foldLeft(matrix)((m, f) => {
      fold(m, f)
    })
    show(folded)
  }

  println(part1(points, instructions))
  part2(points, instructions)
}
