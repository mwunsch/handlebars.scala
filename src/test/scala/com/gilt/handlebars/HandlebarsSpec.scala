package com.gilt.handlebars

import org.scalatest.{ FunSpec, Matchers }

class HandlebarsSpec extends FunSpec with Matchers {
  describe("Issue #36 - Handlebars creation with empty string") {
    val hbs = Handlebars("")
  }
}
