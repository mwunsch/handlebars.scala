package com.gilt.handlebars

import org.specs2.mutable._
import org.specs2.matcher.{ParserMatchers}
import com.gilt.handlebars._

class HandlebarsGrammarSpec extends Specification with ParserMatchers {

  val parsers = new HandlebarsGrammar(("{{","}}"))

  "The Handlebars root grammar" should {
    "parse simple mustaches" in {
      parsers.root("{{foo}}") must haveSuccessResult("""Mustache\(.*foo\)""")
    }

    "parse simple mustaches with whitespace padding" in {
      parsers.root("{{ foo }}") must haveSuccessResult("""Mustache\(.*foo\)""")
    }

    "parse mustaches with parameters" in {
      parsers.root("{{foo bar}}") must haveSuccessResult("""Mustache\(.*foo.*,.*bar.*\)""")
    }

  }
}