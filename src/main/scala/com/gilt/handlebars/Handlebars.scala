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

  def mustache = expression(opt(whiteSpace) ~> ident <~ opt(whiteSpace) ^^ { Mustache(_) })

  def text = rep1(not(openDelimiter) ~> ".|\r|\n".r) ^^ {t => Text(t.mkString("")) }

  def expression[T <: Node](parser: Parser[T]) =
      positioned(openDelimiter ~> parser <~ closeDelimiter)

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Mustache(value: String) extends Node

case class Text(value: String) extends Node
