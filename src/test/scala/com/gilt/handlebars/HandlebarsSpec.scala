package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._

class HandlebarsSpec extends Specification {
  "Handlebars" should {
    "parse a template into an ast" in {
      Handlebars.parse("{{foo}} bar") must beAnInstanceOf[Program]
    }

    "create a Handlebars handler from an ast" in {
      val program = Handlebars.parse("{{foo}} bar")
      val compiler = new Handlebars(program)
      val context = new {val foo = "FOO"}
      compiler(context) must beEqualTo("FOO bar")
    }

    "quickly compile a template" in {
      Handlebars("{{foo}} bar") must beAnInstanceOf[Handlebars]
    }
  }

}
