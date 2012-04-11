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

  def root: Parser[List[Node]] = rep(content | statement)

  def content = rep1(not(openDelimiter | closeDelimiter) ~> ".|\r|\n".r) ^^ {t => Content(t.mkString(""))}

  def statement = mustache | unescapedMustache | section | inverseSection | comment | partial

  def inverseSection = blockify("^") ^^ {case (name,body) => Section(name, body, inverted=true)}

  def section = blockify("#") ^^ {case (name, body) => Section(name, body)}

  def partial = mustachify(">" ~> pad(identifier) ^^ {Partial(_)})

  def comment = mustachify("!" ~> content ^^ {t => Comment(t.value)})

  def unescapedMustache =
      mustachify("{" ~> pad(path) <~ "}" ^^ {Mustache(_, escaped=false)}) |
      mustachify("&" ~> pad(path) ^^ {Mustache(_, escaped=false)})

  def mustache = mustachify(pad(mustachable))

  /* --- */

  def mustachable = helper ^^ { case id ~ list => Mustache(id, list) } | path ^^ {Mustache(_)}

  def helper = identifier ~ rep1(rep1(whiteSpace) ~> path)

  def path = rep1sep(identifier, "/" | ".") ^^ {Path(_)}

  def identifier = (".." | ident) ^^ {Identifier(_)}

  // TODO: rather than a path this should be a mustachable
  // TODO: need to pad() the identity in the closing block
  def blockify(prefix: String) = openDelimiter ~> prefix ~ pad(path) <~ closeDelimiter >> {
    case operation ~ identity => {
      root <~ openDelimiter <~ "/" <~ identity.value.head.value <~ closeDelimiter ^^ { body =>
        (identity, body)
      }
    }
  }

  def mustachify[T <: Node](parser: Parser[T]): Parser[T] =
      positioned(openDelimiter ~> parser <~ closeDelimiter)

  def pad[T <: Node](id: Parser[T]): Parser[T] = padding ~> id <~ padding

  def padding = opt(whiteSpace)

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  override def skipWhitespace = false

}

sealed abstract class Node extends Positional

case class Identifier(value: String) extends Node

case class Path(value: List[Identifier]) extends Node

case class Content(value: String) extends Node

case class Comment(value: String) extends Node

case class Partial(value: Node) extends Node

case class Section(name: Path, value: List[Node], inverted: Boolean = false) extends Node

case class Mustache(value: Node,
    parameters: List[Path] = List.empty,
    escaped: Boolean = true) extends Node
