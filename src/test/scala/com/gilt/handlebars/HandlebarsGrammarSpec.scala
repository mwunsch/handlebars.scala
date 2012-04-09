package com.gilt.handlebars

import org.specs2.mutable._
import org.specs2.matcher.{ParserMatchers}
import com.gilt.handlebars._

class HandlebarsGrammarSpec extends Specification with ParserMatchers {

  "The Handlebars grammar" should {
    "parse a mustache" in {
      parsers.variable("{{foo}}") must haveSuccessResult("""Variable\(foo,false\)""")
    }
  }

  val parsers = new HandlebarsGrammar(("{{","}}"))

}