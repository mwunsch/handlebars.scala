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

  def root = rep(mustache | text)

  def mustache = unescapedVariable | variable

  def unescapedVariable =
      expression("{" ~> identifier <~ "}" ^^ { Variable(_, true) }) |
      expression("&" ~> identifier ^^ { Variable(_, true)})

  def variable = expression(identifier ^^ { Variable(_) })

  def text = rep1(not(openDelimiter) ~> ".|\r|\n".r) ^^ {t => Text(t.mkString("")) }

  def expression[T <: Node](parser: Parser[T]) =
      positioned(openDelimiter ~> parser <~ closeDelimiter)

  def identifier = opt(whiteSpace) ~> ident <~ opt(whiteSpace)

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Variable(value: String, unescape: Boolean = false) extends Node

case class Text(value: String) extends Node
