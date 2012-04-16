package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._

class HandlebarsSpec extends Specification {
  "Handlebars" should {
    "parse a template into an ast" in {
      Handlebars.parse("{{foo}} bar") must beAnInstanceOf[Program]
    }
  }
}