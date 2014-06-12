package com.gilt.handlebars.partial

import org.scalatest.{ FunSpec, Matchers }
import com.gilt.handlebars.parser._
import scala.io.Source
import java.io.File
import com.gilt.handlebars.binding.dynamic._

/**
 * User: chicks
 * Date: 7/1/13
 * Time: 11:22 AM
 */
class PartialHelperSpec extends FunSpec with Matchers {
  import PartialHelper._
  describe("filterPartials") {
    it("filters partials out of a simple template") {
      val program = HandlebarsGrammar("{{> myPartial}}").get
      val expected = List(Partial(PartialName(Identifier(List("myPartial"))),None))

      filterPartials(program) should equal(expected)
    }

    it("filters partials out of a complex template") {
      val template = Source.fromFile("src/test/resources/partialParse.handlebars").mkString
      val program = HandlebarsGrammar(template).get
      val expected = Set(
        Partial(PartialName(Identifier(List("localPartial"))),None),
        Partial(PartialName(Identifier(List("partials", "aPartial"))),None),
        Partial(PartialName(Identifier(List("filetest"))),None)
      )
      filterPartials(program).toSet should equal(expected)
    }
  }

  describe("findPartials") {
    it("finds partial in a file") {
      val partials = findAllPartials(new File("src/test/resources/filetest.handlebars"))
      val expected = Map(
        "localPartial" -> "src/test/resources/localPartial.handlebars",
        "partials/aPartial" -> "src/test/resources/partials/aPartial.handlebars",
        "partialWithinAPartial" -> "src/test/resources/partials/partialWithinAPartial.handlebars"
      )
      val actual = partials.mapValues(_.getPath)

      actual should equal(expected)
    }

    it("recursively finds partials in a file") {
      val partials = findAllPartials(new File("src/test/resources/intermediate.handlebars"))
      val expected = Map(
        "somethingElse" -> "src/test/resources/somethingElse.handlebars",
        "person" -> "src/test/resources/person.handlebars",
        "intermediate" -> "src/test/resources/intermediate.handlebars"
      )
      val actual = partials.mapValues(_.getPath)

      actual should equal(expected)
    }
  }
}
