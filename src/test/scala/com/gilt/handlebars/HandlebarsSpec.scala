package com.gilt.handlebars

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class HandlebarsSpec extends FunSpec with ShouldMatchers {
  describe("Issue #36 - Handlebars creation with empty string") {
    val hbs = Handlebars("")
  }
}
