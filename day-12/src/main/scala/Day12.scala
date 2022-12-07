import cats.implicits._

import scala.annotation.tailrec

object Day12 {

  def part1(input: String): Int =
    Day12Parser.parse(input).search { (node, path) =>
      node.isInstanceOf[Node.BigCave] || !path.contains(node)
    }.size


  def part2(input: String): Int =
    Day12Parser.parse(input).search { (node, path) =>
      val smallCaveVisits =
        path
          .filter(_.isInstanceOf[Node.SmallCave])
          .groupMapReduce(identity)(_ => 1)(_ + _)
      val highestNumberOfVisits =
        smallCaveVisits.values.maxOption.getOrElse(0)
      node.isInstanceOf[Node.BigCave] || node == Node.End ||
        (node.isInstanceOf[Node.SmallCave] && (smallCaveVisits.getOrElse(node, 0) == 0 || highestNumberOfVisits < 2))
    }.size
}

sealed trait Node

object Node {

  case object Start extends Node
  case object End extends Node
  final case class SmallCave(name: String) extends Node
  final case class BigCave(name: String) extends Node
}

final case class Edge(a: Node, b: Node) {
  def contains(node: Node): Boolean =
    a == node || b == node
}

final case class Graph(edges: Vector[Edge]) {

  lazy val nodes: Set[Node] =
    edges.flatMap(e => Vector(e.a, e.b)).toSet

  private val edgeMap: Map[Node, Set[Node]] =
    nodes.map { node =>
      node -> connectedNodes(node)
    }.toMap

  private def connectedNodes(node: Node): Set[Node] =
    edges.flatMap { edge =>
      if (edge.contains(node)) {
        Vector(edge.a, edge.b)
      } else Vector.empty
    }.toSet - node

  def connections(node: Node): Set[Node] =
    if (node == Node.End) Set.empty else edgeMap(node)

  def search(f: (Node, Vector[Node]) => Boolean): Vector[Vector[Node]] = {

    def next(path: Vector[Node]): Vector[Vector[Node]] =
      path.last match {
        case Node.End =>
          Vector(path)
        case _ =>
          connections(path.last)
            .toVector
            .filter(f(_, path))
            .map(path :+ _)
      }

    @tailrec
    def search(paths: Vector[Vector[Node]]): Vector[Vector[Node]] =
      if (paths.forall(_.last == Node.End)) paths else search(paths.flatMap(next))

    search(Vector(Vector(Node.Start)))
  }
}

object Day12Parser {

  import atto._
  import Atto._

  private val start: Parser[Node] =
    string("start").as(Node.Start)

  private val end: Parser[Node] =
    string("end").as(Node.End)

  private val bigCave: Parser[Node] =
    stringOf1(upper).map(Node.BigCave)

  private val smallCave: Parser[Node] =
    stringOf1(lower).map(Node.SmallCave)

  private val node: Parser[Node] =
    start | end | bigCave | smallCave

  private val edge: Parser[Edge] =
    (node <~ char('-'), node).mapN(Edge)

  private val newline = char('\r') | char('\n')

  private val graph: Parser[Graph] =
    edge.sepBy(newline).map(edges => Graph(edges.toVector))

  def parse(input: String): Graph =
    graph.parseOnly(input).done.option.get
}