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
      mustachify("{" ~> pad(path) <~ "}" ^^ {Mustache(_, escaped=false)}) |
      mustachify("&" ~> pad(path) ^^ {Mustache(_, escaped=false)})

  def mustache = mustachify(path ^^ {Mustache(_)}) |
      mustachify(helper ^^ { case id ~ list => Mustache(id, list) })

  def helper = identifier ~ rep1(rep1(whiteSpace) ~> path)

  def path: Parser[Node] = rep1sep(identifier, "/") ^^ {Path(_)}

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

case class Path(value: List[Node]) extends Node

case class Mustache(value: Node,
    parameters: List[Node] = List.empty,
    escaped: Boolean = true) extends Node
