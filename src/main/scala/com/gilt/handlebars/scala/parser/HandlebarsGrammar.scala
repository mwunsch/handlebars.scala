package com.gilt.handlebars.scala.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object HandlebarsGrammar {
  protected val grammar = new HandlebarsGrammar(("{{","}}"))
  def apply(input: String): grammar.ParseResult[Program] = grammar(input)
  def path(input: String): grammar.ParseResult[Identifier] = grammar.parseAll(grammar.path, input)
}

class HandlebarsGrammar(delimiters: (String, String)) extends JavaTokenParsers {
  override def skipWhitespace = false

  def apply(input: String) = {
    if (input.isEmpty) {
      Success(Program(List(Content(input))), new CharSequenceReader(""))
    } else {
      parseAll(root, input)
    }

  }

  def root = program

  def program: Parser[Program] = {
    inverse ~ statements ^^ {
      case _ ~ section => Program(Nil, Some(Program(section)))
    } |
    statements ~ inverse ~ statements ^^ {
      case control ~ _ ~ flip => Program(control, Some(Program(flip)))
    } |
    statements ~ inverse ^^ {
      case section ~ _ => Program(section)
    } |
    statements ^^ { Program(_) } |
    inverse ^^^ { Program(Nil) }
  }

  def statements = rep1(statement)

  def statement = {
    inverseBlock |
    block |
    mustache |
    partial |
    CONTENT ^^ { Content(_) } |
    comment
  }

  def inverseBlock = blockify("^") ^^ {
      case (stache, Some(prog)) => Block(stache, prog.inverse.getOrElse(Program(Nil)), Some(prog))
      case (stache, None) => Block(stache, Program(Nil), None)
    }

  def block = blockify("#") ^^ {
      case (stache, Some(prog)) => Block(stache, prog, prog.inverse)
      case (stache, None) => Block(stache, Program(Nil), None)
    }

  def mustache: Parser[Mustache] = {
    mustachify(pad(inMustache)) ^^ { mustacheable(_) } |
    mustachify("&" ~> pad(inMustache)) ^^ { mustacheable(_, true) } |
    mustachify("{" ~> pad(inMustache) <~ "}") ^^ { mustacheable(_, true) }
  }

  def partial: Parser[Partial] = mustachify(">" ~> pad( partialName ~ opt(whiteSpace ~> path) )) ^^ {
    case (name ~ contextOpt) => Partial(name, contextOpt)
  }

  def inMustache: Parser[(IdentifierNode, List[Either[Mustache, ValueNode]], Option[HashNode])] = {
    path ~ params ~ hash ^^ {
      case (id ~ params ~ hash) => (id, params, Some(hash))
    } |
    path ~ hash ^^ {
      case (id ~ hash) => (id, Nil, Some(hash))
    } |
    path ~ params ^^ {
      case (id ~ params) => (id, params, None)
    } |
    path ^^ { (_ , Nil, None) } |
    dataName ^^ { (_ , Nil, None) } |
    failure("Invalid Mustache")
  }

  def params = rep1(whiteSpace ~> paramOrNested)

  def hash = rep1(whiteSpace ~> hashSegment) ^^ {
    pairs:List[(String, ValueNode)] => HashNode(pairs.toMap)
  }

  def hashSegment = (ID ~ EQUALS ~ param) ^^ {
    case (i ~ _ ~ p) => (i, p)
  }

  def partialName = (path | STRING | INTEGER) ^^ { PartialName(_) }

  def param = STRING |
              INTEGER |
              BOOLEAN |
              path |
              dataName

  def paramOrNested: Parser[Either[Mustache, ValueNode]] = param ^^(Right(_)) | nestedHelperParam ^^(Left(_))

  def nestedHelperParam = "(" ~> (pad(inMustache)^^ { mustacheable(_) })  <~ ")"

  def dataName = "@" ~> not("." | "..") ~> simplePath ^^ { DataNode(_) }

  def path = not("else") ~> rep1sep(ID <~ not(EQUALS) | PARENT | SELF, SEPARATOR) ^^ { Identifier(_) }

  def simplePath = not("else") ~> rep1sep(ID <~ not(EQUALS), SEPARATOR) ^^ { Identifier(_) }

  def inverse = mustachify( pad("^" | "else") )

  def comment = mustachify("!" ~> CONTENT) ^^ { Comment(_) }

  def blockify(prefix: Parser[String]): Parser[(Mustache, Option[Program])] = {
    blockstache(prefix) ~ opt(program) ~ mustachify("/" ~> pad(path)) >> {
      case (mustache ~ _ ~ close) if close != mustache.path => failure(mustache.path.string + " doesn't match " +
close.string)
      case (mustache ~ programOpt ~ _) => success((mustache, programOpt))
    }
  }

  def blockstache(prefix: Parser[String]) = mustachify(prefix ~> pad(inMustache)) ^^ {
    mustacheable(_)
  }

  def mustacheable(tuple: (IdentifierNode, List[Either[Mustache, ValueNode]], Option[HashNode]),
    unescape: Boolean = false): Mustache = {
      tuple match {
        case (id, params, Some(hash)) => Mustache(id, params, hash, unescape)
        case (id, params, None) => Mustache(id, params, unescaped = unescape)
      }
  }

  def mustachify[T](parser: Parser[T]): Parser[T] = OPEN ~> parser <~ CLOSE

  def pad[T](id: Parser[T]): Parser[T] = opt(whiteSpace) ~> id <~ opt(whiteSpace)

  val STRING = stringLiteral ^^ { s:String => StringParameter(s.stripPrefix("\"").stripSuffix("\"")) }

  val INTEGER = wholeNumber ^^ { n:String => IntegerParameter(n.toInt) }

  val BOOLEAN = {
    "true" ^^^ { BooleanParameter(true) } |
    "false" ^^^ { BooleanParameter(false) }
  }

  val EQUALS = "="

  val ID = """[^\s!"#%-,\.\/;->@\[-\^`\{-~]+""".r | ("[" ~> """[^\]]*""".r <~ "]") | ident

  val SEPARATOR = "/" | "."

  val PARENT = ".."

  val SELF = "."

  val OPEN = delimiters._1

  val CLOSE = delimiters._2

  val ESCAPE = "\\"

  val CONTENT = rep1((ESCAPE ~> (OPEN | CLOSE) | not(OPEN | CLOSE) ~> ".|\r|\n".r)) ^^ { t => t.mkString("") }
}
