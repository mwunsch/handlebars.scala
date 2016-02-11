package com.gilt.handlebars.scala.parser

abstract class Node
trait ValueNode extends Node {
  type T
  val value: T
}

abstract class ParameterNode extends ValueNode

trait IdentifierNode extends ValueNode {
  type T = Seq[String]
  val isSimple: Boolean
  def string = value.mkString(".")
}


case class Program(statements: Seq[Node], inverse: Option[Program] = None) extends Node

case class Mustache(path: IdentifierNode,
    params: Seq[Either[Mustache, ValueNode]] = Nil,
    hash: HashNode = HashNode(Map.empty),
    unescaped: Boolean = false) extends Node {
  val eligibleHelper: Boolean = path.isSimple
  val isHelper: Boolean = eligibleHelper && params.nonEmpty
}

case class Partial(name: PartialName, context: Option[Identifier] = None) extends Node

case class Block(mustache: Mustache,
  program: Program,
  inverse: Option[Program] = None) extends Node

case class Content(value: String) extends Node

case class HashNode(value: Map[String, ValueNode]) extends ValueNode {
  type T = Map[String, ValueNode]
}

case class Identifier(parts: Seq[String]) extends IdentifierNode {
  val value = parts
  val isSimple: Boolean = parts.length == 1
}

case class PartialName(value: ValueNode) extends ValueNode {
  type T = ValueNode
}

case class DataNode(id: Identifier) extends IdentifierNode {
  val value = id.value
  val isSimple = id.isSimple
}

case class StringParameter(value: String) extends ParameterNode {
  type T = String
}
case class IntegerParameter(value: Int) extends ParameterNode {
  type T = Int
}
case class BooleanParameter(value: Boolean) extends ParameterNode {
  type T = Boolean
}

case class Comment(value: String) extends ValueNode {
  type T = String
}

