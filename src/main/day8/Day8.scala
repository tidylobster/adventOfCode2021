package main.day8

import main.Utils

case class Pattern(input: Seq[String]) {
  // unique digits
  lazy val one: Set[Char] = input.filter(_.length == 2).head.toSet
  lazy val four: Set[Char] = input.filter(_.length == 4).head.toSet
  lazy val seven: Set[Char] = input.filter(_.length == 3).head.toSet
  lazy val eight: Set[Char] = input.filter(_.length == 7).head.toSet

  // deducted from unique digits
  lazy val six: Set[Char] = input.filter(_.length == 6).map(_.toSet).filter(x => seven.diff(x).nonEmpty).head
  lazy val nine: Set[Char] = input.filter(_.length == 6).map(_.toSet).filter(_.diff(four).size == 2).head
  lazy val zero: Set[Char] = input.filter(_.length == 6).map(_.toSet).filterNot(_.equals(six)).filterNot(_.equals(nine)).head

  // segments
  lazy val upperSegment: Char = seven.diff(one).head
  lazy val upperRightSegment: Char = eight.diff(six).head
  lazy val upperLeftSegment: Char = four.diff(one ++ Set(middleSegment)).head
  lazy val middleSegment: Char = eight.diff(zero).head
  lazy val lowerRightSegment: Char = one.diff(Set(upperRightSegment)).head
  lazy val lowerLeftSegment: Char = eight.diff(nine).head
  lazy val bottomSegment: Char = nine.diff(four ++ Set(upperSegment)).head

  def decode(input: String): Int = input.toSet match {
    case this.one => 1
    case x if x == Set(upperSegment, upperRightSegment, middleSegment, lowerLeftSegment, bottomSegment) => 2
    case x if x == Set(upperSegment, upperRightSegment, middleSegment, lowerRightSegment, bottomSegment) => 3
    case this.four => 4
    case x if x == Set(upperSegment, upperLeftSegment, middleSegment, lowerRightSegment, bottomSegment) => 5
    case this.six => 6
    case this.seven => 7
    case this.eight => 8
    case this.nine => 9
    case this.zero => 0
  }

  def decode(input: Seq[String]): String =
    input.map(decode).mkString
}

object Day8 extends App with Utils {
  val data = getData("src/resources/Day8.txt").split("\n")
  val data1 = data.map(_.split(" \\| ")(1)).flatMap(_.split(" ").toList)
  val data2 = data.map(_.split(" \\| ").toList).toList

  def part1(input: Seq[String]): Int =
    input.map {
      case x if Seq(2,3,4,7).contains(x.length) => 1
      case _ => 0
    }.sum

  def part2(input: Seq[Seq[String]]): Int =
    input.map { x =>
      val pattern = Pattern(x.head.split(" ").toList)
      pattern.decode(x.last.split(" ").toList).toInt
    }.sum

  println(part1(data1))
  println(part2(data2))
}
