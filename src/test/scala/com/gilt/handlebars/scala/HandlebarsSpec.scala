package com.gilt.handlebars.scala

import com.gilt.handlebars.scala.binding.dynamic._
import org.scalatest.{FunSpec, Matchers}

class HandlebarsSpec extends FunSpec with Matchers {
  describe("Issue #36 - Handlebars creation with empty string") {
    val hbs = Handlebars("")
  }

  describe("Handlebars strict rendering") {
    it("should return errors when path missing") {
      val hbs = Handlebars("Hello {{name}}")
      val result = hbs.renderStrict(Map("nme" -> "world"))
      result match {
        case Right(_) => fail
        case Left(errors) =>
          errors.head should include("Could not find path or helper: Identifier(List(name))")
      }
    }

    it("should render when there are no errors") {
      val hbs = Handlebars("Hello {{name}}")
      val result = hbs.renderStrict(Map("name" -> "world"))
      result match {
        case Right(s) => s shouldEqual "Hello world"
        case Left(errors) => fail
      }
    }
  }
}
