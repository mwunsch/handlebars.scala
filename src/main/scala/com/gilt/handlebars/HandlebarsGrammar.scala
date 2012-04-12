package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional}

class HandlebarsGrammar(delimiters: (String, String) = ("{{", "}}")) extends JavaTokenParsers {

  def root: Parser[Program] = rep(content | statement) ^^ {Program(_)}

  def content =
      positioned(rep1(not(openDelimiter | closeDelimiter) ~> ".|\r|\n".r) ^^ {
        t => Content(t.mkString(""))
      })

  def statement = mustache | unescapedMustache | section | inverseSection | comment | partial

  def inverseSection = positioned(blockify("^") ^^ {
    case (name,body) => Section(name, body, inverted=true)
  })

  def section = positioned(blockify("#") ^^ {case (name, body) => Section(name, body)})

  def partial = mustachify(">" ~> pad(identifier) ^^ {Partial(_)})

  def comment = mustachify("!" ~> content ^^ {t => Comment(t.value)})

  def unescapedMustache =
      mustachify("{" ~> pad(path) <~ "}" ^^ {Mustache(_, escaped=false)}) |
      mustachify("&" ~> pad(path) ^^ {Mustache(_, escaped=false)})

  def mustache = mustachify(pad(mustachable))

  /* --- */

  def mustachable = helper ^^ { case id ~ list => Mustache(id, list) } | path ^^ {Mustache(_)}

  def helper = path ~ rep1(rep1(whiteSpace) ~> path)

  def path = rep1sep(identifier, "/" | ".") ^^ {Path(_)}

  def identifier = (".." | ident) ^^ {Identifier(_)}

  // TODO: need to pad() the identity in the closing block
  def blockify(prefix: String) = openDelimiter ~> prefix ~> pad(mustachable) <~ closeDelimiter >> {
    case (identity: Mustache) => {
      val path: Path = identity.value
      pad(root) <~ openDelimiter <~ "/" <~ path.head.value <~ closeDelimiter ^^ { body =>
        (path, body)
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

case class Path(value: List[Identifier]) extends Node {
  def head: Identifier = value.head
}

case class Content(value: String) extends Node

case class Comment(value: String) extends Node

case class Partial(value: Node) extends Node

case class Section(name: Path, value: Program, inverted: Boolean = false) extends Node

case class Program(value: List[Node]) extends Node

case class Mustache(value: Path,
    parameters: List[Path] = List.empty,
    escaped: Boolean = true) extends Node
