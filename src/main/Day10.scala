package main

import scala.collection.mutable

object Day10 extends App with Utils {
  val data = getData("src/resources/Day10.txt").split("\n")

  val reversedMap = Map(
    '[' -> ']', ']' -> '[',
    '{' -> '}', '}' -> '{',
    '(' -> ')', ')' -> '(',
    '<' -> '>', '>' -> '<'
  )

  val syntaxScoreMap = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val autocompleteScoreMap = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L
  )

  def getComplement(chunk: Char): Char =
    reversedMap.getOrElse(chunk, throw new IllegalArgumentException(s"Unsupported chunk: $chunk"))

  private def isClosed(chunk: Char): Boolean = !isOpen(chunk)
  private def isOpen(chunk: Char): Boolean = chunk match {
    case '[' | '(' | '{' | '<' => true
    case ']' | ')' | '}' | '>' => false
  }

  def scan(input: Seq[String]): Seq[Either[Char, mutable.Stack[Char]]] =
    input.map { line =>
      val stack = mutable.Stack[Char]()
      val corrupted = line.dropWhile {
        case chunk if isOpen(chunk) => stack.push(chunk); true
        case chunk if isClosed(chunk) => stack.top match {
          case top if isClosed(top) => stack.push(chunk); true
          case top if isOpen(top) && getComplement(top) == chunk => stack.pop(); true
          case _ => false
        }
      }.headOption

      if (corrupted.isDefined)
        Left(corrupted.get)
      else
        Right(stack)
    }

  def part1(input: Seq[String]): Int =
    scan(input)
      .filter(_.isLeft)
      .map(_.left.get)
      .map(syntaxScoreMap.getOrElse(_, Int.MinValue))
      .sum

  def part2(input: Seq[String]): Long = {
    val scores = scan(input)
      .filter(_.isRight)
      .map(_.right.get)
      .map(_.map(getComplement).toList)
      .map(_.foldLeft(0L)((score, char) => score * 5L + autocompleteScoreMap.getOrElse(char, Long.MinValue)))
      .sorted

    scores(scores.length / 2)
  }

  println(part1(data))
  println(part2(data))
}
