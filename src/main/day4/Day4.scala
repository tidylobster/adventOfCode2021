package main.day4

import main.Utils
import scala.annotation.tailrec

case class Number(value: Int, mark: Boolean)
case class Board(rows: Seq[Seq[Number]], lastDraw: Option[Int] = None) {
  lazy val hasWon: Boolean =
    rows.map(_.forall(_.mark)).exists(identity) ||
      rows.transpose.map(_.forall(_.mark)).exists(identity)

  def score: Int = rows.map(_.filterNot(_.mark).map(_.value).sum).sum * lastDraw.getOrElse(0)

  def update(input: Int): Board = Board(
    rows = rows.map(_.map(n => if (n.value == input) Number(input, mark = true) else n)),
    lastDraw = Some(input)
  )
}

object Board {
  def from(input: Seq[String]): Board = {
    Board(input
      .map(_
        .trim
        .split(" ")
        .filterNot(_.trim == "")
        .map(x => Number(x.toInt, mark = false))
        .toSeq
      )
    )
  }
}

object Day4 extends App with Utils {
  val data: Seq[String] = getData("src/resources/Day4.txt").split("\n").toList
  val drawings: Seq[Int] = data.head.split(",").toList.map(_.toInt)
  var boards: Seq[Board] = data.tail.filterNot(_ == "").grouped(5).toList.map(Board.from)

  def part1(drawings: Seq[Int], bingo: Seq[Board]): Int = {
    @tailrec
    def loop(draws: Seq[Int], boards: Seq[Board]): Option[Board] = draws match {
      case x :: xs =>
        val updatedBoards = boards.map(_.update(x))
        val candidates = updatedBoards.filter(_.hasWon)
        if (candidates.nonEmpty)
          candidates.headOption
        else
          loop(xs, updatedBoards)
      case Nil => None
    }
    loop(drawings, bingo).get.score
  }

  def part2(drawings: Seq[Int], bingo: Seq[Board]): Int = {
    @tailrec
    def loop(draws: Seq[Int], boards: Seq[Board], lastWon: Option[Board]): Option[Board] = draws match {
      case x :: xs =>
        val updatedBoards = boards.map(_.update(x))
        val (winners, losers) = updatedBoards.partition(_.hasWon)
        if (winners.nonEmpty)
          loop(xs, losers, winners.lastOption)
        else
          loop(xs, losers, lastWon)
      case Nil => lastWon
    }
    loop(drawings, bingo, None).get.score
  }

  println(part1(drawings, boards))
  println(part2(drawings, boards))
}
