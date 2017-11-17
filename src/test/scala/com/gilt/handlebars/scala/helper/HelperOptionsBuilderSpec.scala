package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import org.scalatest.{FunSpec, Matchers}

class HelperOptionsBuilderSpec extends FunSpec with Matchers {
  describe("HelperOptions[T].arguments") {
    it("should contain all provided arguments") {
      val data = Map("three" -> "3")
      val content = """{{join "one" 2 three}}"""
      val helper = Helper[Any] { (_, options) =>
        options.arguments.map(_.render).mkString(",")
      }
      val builder = Handlebars.createBuilder(content).withHelpers(Map("join" -> helper))
      builder.build(data) should be("one,2,3")
    }
  }
}