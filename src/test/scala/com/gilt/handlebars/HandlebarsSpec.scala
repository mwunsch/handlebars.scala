package com.gilt.handlebars

import org.specs2.mutable._
import java.io.File

class HandlebarsSpec extends Specification {
  "Handlebars" should {
    "parse a template into an ast" in {
      Handlebars.parse("{{foo}} bar") must beAnInstanceOf[Program]
    }

    "create a Handlebars handler from an ast" in {
      val program = Handlebars.parse("{{foo}} bar")
      val compiler = Handlebars(program)
      val context = new {val foo = "FOO"}
      compiler(context) must beEqualTo("FOO bar")
    }

    "quickly compile a template" in {
      Handlebars("{{foo}} bar") must beAnInstanceOf[Handlebars]
    }

    "create a Handlebars from a file" in {
      val fileTest = new File("src/test/resources/filetest.handlebars")
      val template = Handlebars.fromFile(fileTest)

      template(new {}) must beEqualTo(
      """This is a handlebars file.
        |
        |This is a local partial
        |
        |localPartial
        |
        |This is a partial in directory
        |
        |Nested Partial: partialWithinAPartial""".stripMargin
      )
    }

    "fail to find file to create template" in {
      val fileTest = new File("src/test/resources/doesNotExist.handlebars")

      Handlebars.fromFile(fileTest) must throwAn[IllegalArgumentException]
    }

    "fail to find partial while creating a template" in {
      val fileTest = new File("src/test/resources/missingPartial.handlebars")

      Handlebars.fromFile(fileTest) must throwAn[IllegalArgumentException]
    }
  }

}
