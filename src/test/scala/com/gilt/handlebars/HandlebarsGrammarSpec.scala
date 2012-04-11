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

    "parse an unescaped mustache" in {
      parsers.root("{{{foo}}}") must haveSuccessResult(""".*foo.*,.*false.*""")
    }

    "parse an unescaped mustache with the ampersand" in {
      parsers.root("{{& foo}}") must haveSuccessResult(""".*foo.*,.*false.*""")
    }

    "parse a mustache with a path" in {
      parsers.root("{{foo/bar}}") must haveSuccessResult(""".*foo.*,.*bar.*""")
    }

    "parse a mustache with a dot to narrow the context" in {
      parsers.root("{{foo.bar}}") must haveSuccessResult(""".*foo.*,.*bar.*""")
    }

    "parse a mustache with two dots to specify the higher level context" in {
      parsers.root("{{../foo}}") must haveSuccessResult(""".*\.\..*foo.*""")
    }

    "parse contents followed by a mustache" in {
      parsers.root("foo bar {{baz}}") must beASuccess
    }

    "parse a comment" in {
      parsers.root("{{! this is a comment }}") must haveSuccessResult("Comment.*comment")
    }

    "parse a multi-line comment" in {
      parsers.root("{{!\nthis is a multi-line comment\n}}") must beASuccess
    }

    "parse a partial" in {
      parsers.root("{{> foo }}") must haveSuccessResult("Partial")
    }

    "parse a section" in {
      parsers.root("{{#foo}} bar {{/foo}}") must haveSuccessResult("Section")
    }

    "parse an inverse section" in {
      parsers.root("{{^foo}} bar {{/foo}}") must haveSuccessResult("Section")
    }

    "parse a nested section" in {
      parsers.root("{{#foo}} bar {{#baz}} nested! {{/baz}} bar {{/foo}}") must beASuccess
    }


  }
}