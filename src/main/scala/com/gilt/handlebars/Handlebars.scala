package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional}

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")) = {
    val grammar = new HandlebarsGrammar(delimiters)
    grammar.parseAll(grammar.root, template)
  }

}

class HandlebarsGrammar(delimiters: (String, String)) extends JavaTokenParsers {

  def root = rep(mustache)

  def mustache = mustachify(identifier ^^ {Mustache(_)}) |
      mustachify(identifier ~ rep1(rep(whiteSpace) ~> identifier) ^^ {
        case id ~ list => Mustache(id, list)
      })

  def identifier = ident ^^ {Identifier(_)}

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  def mustachify[T <: Node](parser: Parser[T]) =
      positioned(openDelimiter ~> opt(whiteSpace) ~> parser <~ opt(whiteSpace) <~ closeDelimiter)

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Identifier(value: String) extends Node

case class Mustache(value: Identifier,
    parameters: List[Identifier] = List.empty,
    escaped: Boolean = true) extends Node
