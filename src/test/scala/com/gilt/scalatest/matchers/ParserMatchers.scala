package com.gilt.scalatest.matchers

trait ParserMatchers {
  import scala.util.parsing.combinator.Parsers
  import scala.util.parsing.input._
  import org.scalatest.matchers._

  val parsers: Parsers
  import parsers.{ParseResult, Success, NoSuccess, Input, Parser}

  def successful = BePropertyMatcher[ParseResult[_]] { (left: ParseResult[_]) =>
    BePropertyMatchResult(left.successful, "successful")
  }
  def success = BePropertyMatcher[ParseResult[_]] { (left: ParseResult[_]) =>
    BePropertyMatchResult(left.successful, "success")
  }
  def unsuccessful[_] = BePropertyMatcher[ParseResult[_]] { (left: ParseResult[_]) =>
    BePropertyMatchResult(!left.successful, "unsuccessful")
  }

  def succeedOn(input: Input) = Matcher[Parser[_]] { (left: Parser[_]) =>
    val result = left(input)
    result match {
      case Success(_, _) => MatchResult(
        true,
        "The parse resulted in a success",
        "The result was successful"
      )
      case NoSuccess(msg, _) => MatchResult(
        false,
        msg,
        "The parse resulted in a failure"
      )
    }
  }

  def failOn(input: Input) = Matcher[Parser[_]] { (left: Parser[_]) =>
    val result = left(input)
    result match {
      case Success(_, _) => MatchResult(
        false,
        "The parse resulted in a success",
        "The result was successful"
      )
      case NoSuccess(msg, _) => MatchResult(
        true,
        msg,
        "The parse resulted in a failure"
      )
    }
  }

  def succeedWithResult[T](expectation: T)(implicit m:Manifest[T]) = Matcher[ParseResult[T]] { (left: ParseResult[T]) =>
    left match {
      case Success(res, _) => MatchResult(
        res == expectation,
        "Expected result "+expectation.toString+", but got "+res.toString,
        "The parse successfully returned "+res.toString
      )
      case NoSuccess(_, _) => MatchResult(
        false,
        "Expected "+expectation.toString+", but the parse did not succeed\n"+left,
        "The parse resulted in a failure"
      )
    }
  }

  def failWithMessage(expectation: String) = Matcher[ParseResult[_]] { (left: ParseResult[_]) =>
    left match {
      case Success(_, _) => MatchResult(
        false,
        "The parse resulted in a success.",
        "The result was successful."
      )
      case NoSuccess(msg, next) => MatchResult(
        expectation.r.findFirstIn(msg).isDefined,
        "Expected the failure to match message:\n"+expectation+"\n  but got:\n"+msg,
        "The parse was unsuccessful and matched "+expectation
      )
    }
  }

  def failAtPosition(line: Int, column: Int) = Matcher[ParseResult[_]] { (left: ParseResult[_]) =>
    left match {
      case Success(_, _) => MatchResult(
        false,
        "The parse resulted in a success.",
        "The result was successful."
      )
      case NoSuccess(_, next) => MatchResult(
        next.pos.line == line && next.pos.column == column,
        "Expected a failure at position ["+line+"."+column+"], but failure was at: ["+next.pos+"]",
        "The parse was unsuccessful at position ["+line+"."+column+"]"
      )
    }
  }

  def failAtLine(line: Int) = Matcher[ParseResult[_]] { (left: ParseResult[_]) =>
    left match {
      case Success(_, _) => MatchResult(
        false,
        "The parse resulted in a success.",
        "The result was successful."
      )
      case NoSuccess(_, next) => MatchResult(
        next.pos.line == line,
        "Expected a failure at line "+line+", but failure was at "+next.pos.line,
        "The parse was unsuccessful at line "+line
      )
    }
  }

  implicit def useStringAsInput(s: String): Reader[Char] = new CharSequenceReader(s)
}


