package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional}

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")) = {
    val grammar = new HandlebarsGrammar(delimiters)
    grammar.parseAll(grammar.root, template)
  }

}

class HandlebarsGrammar(delimiters: (String, String) = ("{{", "}}")) extends JavaTokenParsers {

  def root = rep(statement)

  def statement = mustache | unescapedMustache

  def unescapedMustache =
      mustachify("{" ~> pad(identifier) <~ "}" ^^ {Mustache(_, escaped=false)}) |
      mustachify("&" ~> pad(identifier) ^^ {Mustache(_, escaped=false)})

  def mustache = mustachify(identifier ^^ {Mustache(_)}) |
      mustachify(helperCall ^^ { case id ~ list => Mustache(id, list) })

  def helperCall = identifier ~ rep1(rep(whiteSpace) ~> identifier)

  // TODO: This causes a stack overflow
  def path: Parser[Node] = (path <~ "/") ~ identifier ^^ {case a ~ b => Path(Pair(a,b))} | identifier

  def identifier = (higherLevelPath | currentPath | ident) ^^ {Identifier(_)}

  def padding = opt(whiteSpace)

  def higherLevelPath = """\.\.""".r

  def currentPath = """\.""".r

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  def mustachify[T <: Node](parser: Parser[T]) =
      positioned(openDelimiter ~> pad(parser) <~ closeDelimiter)

  def pad(id: Parser[Node]) = padding ~> id <~ padding

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Identifier(value: String) extends Node

case class Path(value: (Node, Node)) extends Node

case class Mustache(value: Node,
    parameters: List[Node] = List.empty,
    escaped: Boolean = true) extends Node
