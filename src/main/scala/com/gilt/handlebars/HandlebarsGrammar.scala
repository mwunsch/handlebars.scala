package com.gilt.handlebars

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional, Position}

abstract class Node extends Positional

case class Content(value: String) extends Node
case class Identifier(value: String) extends Node
case class Inversion() extends Node
abstract class Argument extends Node
case class Path(value: List[Identifier]) extends Argument {
  def head: Identifier = value.head
  def tail: List[Identifier] = value.tail
}
case class Comment(value: String) extends Node
case class Partial(value: Path) extends Node
case class StringLiteral(value: String) extends Argument
case class DoubleLiteral(value: Double) extends Argument
case class LongLiteral(value: Long) extends Argument
case class Mustache(value: Path,
    parameters: List[Argument] = Nil,
    escaped: Boolean = true) extends Node
case class Section(name: Mustache, value: Program, inverted: Boolean = false) extends Node
case class Program(value: List[Node], inverse: Option[Program] = None) extends Node
case class KeyValue(ident: Identifier, lit: Argument) extends Argument
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

  def mustache = not(elseStache) ~> mustachify(pad(mustachable))

  def elseStache = mustachify(pad("else" | "^") ^^ { _ => new Inversion()})

  def mustachable = helper ^^ { case id ~ list => Mustache(id, list) } | path ^^ {Mustache(_)}

  def helper = path ~ rep1(rep1(whiteSpace) ~> argument)

  def stringLit = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r  <~ "\"" ^^ {StringLiteral(_)}

  def longLit = wholeNumber ^^ {s => LongLiteral(s.toInt) }

  def doubleLit = """(\d+\.(\d*)?|\d*\.\d+)""".r ^^ {s => DoubleLiteral(s.toDouble)}

  def lit = stringLit | doubleLit | longLit

  def keyValue = identifier ~ "=" ~ lit ^^ { case a ~ b ~ c => KeyValue(a, c) }

  def argument =  keyValue | path | lit

  def path = rep1sep(identifier, "/" | ".") ^^ {Path(_)}

  def identifier = (".." | ident) ^^ {Identifier(_)}

  def blockify(prefix: String) = mustachify(prefix ~> pad(mustachable)) >> {
    case (stache: Mustache) => {
      val path: Path = stache.value
      pad(elseBlock | root) <~ openDelimiter <~ "/"<~ pad(path.value.map(_.value).mkString("/")) <~ closeDelimiter ^^ { body =>
        (stache, body)
      }
    }
  }

  def elseBlock = (root ~ elseStache ~ root) ^^ { case (prog ~ stache ~ inversion) => Program(prog.value, Some(inversion)) }

  def mustachify[T <: Node](parser: Parser[T]): Parser[T] =
      positioned(openDelimiter ~> parser <~ closeDelimiter)

  def pad[T](id: Parser[T]): Parser[T] = padding ~> id <~ padding

  def padding = opt(whiteSpace)

  def openDelimiter = delimiters._1

  def closeDelimiter = delimiters._2

  override def skipWhitespace = false

}


