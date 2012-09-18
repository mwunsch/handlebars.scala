package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional, Position}

abstract class Node extends Positional

case class Content(value: String) extends Node
case class Identifier(value: String) extends Node
case class Path(value: List[Identifier]) extends Node {
  def head: Identifier = value.head
  def tail: List[Identifier] = value.tail
}
case class Comment(value: String) extends Node
case class Partial(value: Path) extends Node
case class Mustache(value: Path,
    parameters: List[Path] = Nil,
    escaped: Boolean = true) extends Node
case class Section(name: Mustache, value: Program, inverted: Boolean = false) extends Node
case class Program(value: List[Node]) extends Node

case class InvalidSyntaxException(msg: String, pos: Position) extends RuntimeException(msg)

object HandlebarsGrammar {
  def apply(delimiters: (String,String) = ("{{","}}")) = new HandlebarsGrammar(delimiters)
}

class HandlebarsGrammar(delimiters: (String, String)) extends JavaTokenParsers {

  def scan(in: String): Program = {
    parseAll(root, in) match {
      case Success(result, _) => result
      case NoSuccess(msg, next) => throw new InvalidSyntaxException(msg, next.pos)
    }
  }

  def root: Parser[Program] = rep(content | statement) ^^ {Program(_)}

  def statement = mustache | unescapedMustache | section | inverseSection | comment | partial

  def content =
      positioned(rep1(not(openDelimiter | closeDelimiter) ~> ".|\r|\n".r) ^^ {
        t => Content(t.mkString(""))
      })

  def inverseSection = positioned(blockify("^") ^^ {
    case (name,body) => Section(name, body, inverted=true)
  })

  def section = positioned(blockify("#") ^^ {case (name, body) => Section(name, body)})

  def partial = mustachify(">" ~> pad(path) ^^ {Partial(_)})

  def comment = mustachify("!" ~> content ^^ {t => Comment(t.value)})

  def unescapedMustache =
      mustachify("{" ~> pad(path) <~ "}" ^^ {Mustache(_, escaped=false)}) |
      mustachify("&" ~> pad(path) ^^ {Mustache(_, escaped=false)})

  def mustache = mustachify(pad(mustachable))

  def mustachable = helper ^^ { case id ~ list => Mustache(id, list) } | path ^^ {Mustache(_)}

  def helper = path ~ rep1(rep1(whiteSpace) ~> path)

  def path = rep1sep(identifier, "/" | ".") ^^ {Path(_)}

  def identifier = (".." | ident) ^^ {Identifier(_)}

  def blockify(prefix: String) = mustachify(prefix ~> pad(mustachable)) >> {
    case (stache: Mustache) => {
      val path: Path = stache.value
      pad(root) <~ openDelimiter <~ "/"<~ pad(path.value.map(_.value).mkString("/")) <~ closeDelimiter ^^ { body =>
        (stache, body)
      }
    }
  }

  def mustachify[T <: Node](parser: Parser[T]): Parser[T] =
      positioned(openDelimiter ~> parser <~ closeDelimiter)

  def pad[T](id: Parser[T]): Parser[T] = padding ~> id <~ padding

  def padding = opt(whiteSpace)

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  override def skipWhitespace = false

}


