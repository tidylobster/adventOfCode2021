package main.day12

import scala.collection.mutable
import main.Utils

case class Node(name: String, siblings: mutable.Set[Node])

case class Graph(fromNodes: mutable.Map[String, Node], toNodes: mutable.Map[String, Node]) {
  def getSiblings(node: Node): mutable.Set[Node] = getSiblings(node.name)
  def getSiblings(name: String): mutable.Set[Node] =
    fromNodes.get(name).map(_.siblings).getOrElse(mutable.Set.empty) ++
      toNodes.get(name).map(_.siblings).getOrElse(mutable.Set.empty)
}

object Day12 extends App with Utils {
  type Edge = (String, String)

  private val data: Seq[String] = getData("src/resources/Day12.txt").split("\n")
  private val edges: Seq[Edge] = data.map(_.split("-")).map {
    case Array(left, "start") => ("start", left)
    case Array("end", right) => (right, "end")
    case Array(left, right) => (left, right)
  }

  private val fromNodes: mutable.Map[String, Node] = mutable.Map[String, Node]()
  for (edge <- edges) {
    val fromNode = fromNodes.getOrElseUpdate(edge._1, Node(edge._1, mutable.Set.empty))
    val toNode = fromNodes.getOrElseUpdate(edge._2, Node(edge._2, mutable.Set.empty))
    fromNode.siblings.add(toNode)
  }

  private val toNodes: mutable.Map[String, Node] = mutable.Map[String, Node]()
  for (edge <- edges) {
    val fromNode = toNodes.getOrElseUpdate(edge._1, Node(edge._1, mutable.Set.empty))
    val toNode = toNodes.getOrElseUpdate(edge._2, Node(edge._2, mutable.Set.empty))
    toNode.siblings.add(fromNode)
  }

  private def discover(graph: Graph): Seq[Seq[Node]] = {
    def loop(node: Node, visited: Seq[Node]): Seq[Seq[Node]] = node match {
      case Node("end", _) => Seq(visited :+ node)
      case _ =>
        val smallCaves = visited.filter(isSmallCave)
        val ways = graph.getSiblings(node).filterNot(w => smallCaves.exists(_.name == w.name))
        ways.flatMap(way => loop(way, visited :+ node)).toSeq
    }
    loop(graph.fromNodes("start"), Seq.empty)
  }

  private def isSmallCave(node: Node): Boolean =
    node.name.toLowerCase == node.name

  private def discoverWithRest(graph: Graph): Seq[Seq[Node]] = {
    def loop(node: Node, visited: Seq[Node], rested: Option[Node]): Seq[Seq[Node]] = node match {
      case Node("end", _) => Seq(visited :+ node)
      case _ =>
        val smallCaves = visited.filter(isSmallCave)
        val siblings = graph.getSiblings(node)
        val initialWays = siblings.filterNot(w => smallCaves.exists(_.name == w.name))

        val extendedWays =
          if (rested.isDefined && visited.count(_.name == rested.get.name) < 2 && siblings.exists(_.name == rested.get.name))
            initialWays + rested.get
          else
            initialWays

        val additional =
          if (node.name != "start" && rested.isEmpty && isSmallCave(node))
            extendedWays.flatMap(way => loop(way, visited :+ node, Some(node))).toSeq
          else
            Seq.empty

        extendedWays.flatMap(way => loop(way, visited :+ node, rested)).toSeq ++ additional
    }
    loop(graph.fromNodes("start"), Seq.empty, rested = None)
  }

  def part1(fromNodes: mutable.Map[String, Node], toNodes: mutable.Map[String, Node]): Int =
    discover(Graph(fromNodes, toNodes)).size

  def part2(fromNodes: mutable.Map[String, Node], toNodes: mutable.Map[String, Node]): Int =
    discoverWithRest(Graph(fromNodes, toNodes)).size

  println(part1(fromNodes, toNodes))
  println(part2(fromNodes, toNodes))
}
