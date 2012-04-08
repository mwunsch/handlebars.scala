package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional}

object Handlebars {

  def parse(template: String) = {
    val grammar = new HandlebarsGrammar
    grammar.parseAll(grammar.root, template)
  }

}

class HandlebarsGrammar extends JavaTokenParsers {

  def root = rep(mustache | text)

  def mustache = expression(opt(whiteSpace) ~> ident <~ opt(whiteSpace) ^^ { Mustache(_) })

  def expression[T <: Node](parser: Parser[T]) = positioned(openStache ~> parser <~ closeStache)

  def text = rep1(not(openStache) ~> ".|\r|\n".r) ^^ {t => Text(t.mkString("")) }

  def openStache = "{{"

  def closeStache = "}}"

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Mustache(value: String) extends Node

case class Text(value: String) extends Node
