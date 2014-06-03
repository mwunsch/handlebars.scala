package com.gilt.handlebars.visitor

import org.scalatest.{ FunSpec, Matchers }
import com.gilt.handlebars.Handlebars
import java.io.File
import com.gilt.handlebars.parser.TemplateNotFoundException
import com.gilt.handlebars.context.Person
import com.gilt.handlebars.DynamicBinding._

class PartialSpec extends FunSpec with Matchers {

  describe("partials") {
    it("basic partials") {
      case class Dude(name: String, url: String)
      case class Dudes(dudes: List[Dude])
      val template = "Dudes: {{#dudes}}{{> dude}}{{/dudes}}"
      val partial = Handlebars("{{name}} ({{url}}) ")
      val ctx = Dudes(List(Dude("Yehuda", "http://yehuda"), Dude("Alan", "http://alan")))

      Handlebars(template).apply(ctx, Map.empty, Map("dude" -> partial)) should equal("Dudes: Yehuda (http://yehuda) Alan (http://alan) ")
    }

    it("partials with context") {
      case class Dude(name: String, url: String)
      case class Dudes(dudes: List[Dude])
      val template = "Dudes: {{>dude dudes}}"
      val partial = Handlebars("{{#this}}{{name}} ({{url}}) {{/this}}")
      val ctx = Dudes(List(Dude("Yehuda", "http://yehuda"), Dude("Alan", "http://alan")))
      Handlebars(template).apply(ctx, Map.empty, Map("dude" -> partial)) should equal("Dudes: Yehuda (http://yehuda) Alan (http://alan) ")
    }


    it("partial in a partial") {
      case class Dude(name: String, url: String)
      case class Dudes(dudes: List[Dude])
      val template = "Dudes: {{#dudes}}{{>dude}}{{/dudes}}"
      val dudePartial = Handlebars("{{name}} {{> url}} ")
      val urlPartial = Handlebars("<a href='{{url}}'>{{url}}</a>")
      val ctx = Dudes(List(Dude("Yehuda", "http://yehuda"), Dude("Alan", "http://alan")))
      Handlebars(template).apply(ctx, Map.empty, Map("dude" -> dudePartial, "url" -> urlPartial)) should equal("Dudes: Yehuda <a href='http://yehuda'>http://yehuda</a> Alan <a href='http://alan'>http://alan</a> ")
    }

    it("rendering undefined partial yields and empty string") {
      Handlebars("{{> whatever}}").apply(new {}) should equal("")
    }

    ignore("rendering undefined partial throws an exception") {
      // handlebars.scala will return an empty string for the missing partial.
    }

    ignore("rendering function partial in vm mode") {
      // handlebars.scala does not support function partials
    }

    it("a partial preceding a selector") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{>dude}} {{another_dude}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("dude" -> dudePartial)) should equal("Dudes: Jeepers Creepers")
    }

    it("Partials with slash paths") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> shared/dude}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("shared/dude" -> dudePartial)) should equal("Dudes: Jeepers")
    }

    it("Partials with slash and point paths") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> shared/dude.thing}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("shared/dude.thing" -> dudePartial)) should equal("Dudes: Jeepers")
    }

    ignore("Global Partials") {
      // handlebars.scala does not have global partials
    }

    // Really this is testing HandlebarsBuilder.withPartials.
    it("Multiple partial registration") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> shared/dude}} {{> global_test}}"
      val partials = Map (
        "shared/dude" -> Handlebars("{{name}}"),
        "global_test" -> Handlebars("{{another_dude}}")
      )
      val ctx = Dude("Jeepers", "Creepers")
      val builder = Handlebars.createBuilder(template).withPartials(partials)
      builder.build(ctx) should equal("Dudes: Jeepers Creepers")
    }

    // handlebars.scala doesn't actually support integers, but lets make sure the 404 is used as a string
    it("Partials with integer path") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> 404}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("404" -> dudePartial)) should equal("Dudes: Jeepers")
    }

    it("Partials with complex path") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> 404/asdf?.bar}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("404/asdf?.bar" -> dudePartial)) should equal("Dudes: Jeepers")
    }

    it("Partials with escaped") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> [+404/asdf?.bar]}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("+404/asdf?.bar" -> dudePartial)) should equal("Dudes: Jeepers")
    }

    it("Partials with string") {
      case class Dude(name: String, another_dude: String)
      val template = "Dudes: {{> \"+404/asdf?.bar\"}}"
      val dudePartial = Handlebars("{{name}}")
      val ctx = Dude("Jeepers", "Creepers")
      Handlebars(template).apply(ctx, Map.empty, Map("+404/asdf?.bar" -> dudePartial)) should equal("Dudes: Jeepers")
    }
  }

  describe("file partials") {
    it("Complext partials") {
      val filetest = new File("src/test/resources/filetest.handlebars")
      Handlebars(filetest).apply(new {}) should equal(
        """This is a handlebars file.
          |
          |This is a local partial
          |
          |localPartial
          |
          |This is a partial in directory
          |
          |Nested Partial: partialWithinAPartial""".stripMargin)
    }

    it("fail to find partial while creating a template") {
      val filetest = new File("src/test/resources/missingPartial.handlebars")


      intercept[TemplateNotFoundException] {
        Handlebars(filetest).apply(new {})
      }
    }

    it("does not infinite parse partials") {
      trait Data {
        def number: Int
      }
      class DataLeaf(n: Int) extends Data {
        val number = n
      }
      case class DataWrapper(number: Int, data: Data) extends Data
      val hbs = Handlebars(new File("src/test/resources/recursiveMain.handlebars"))
      val ctx = DataWrapper(1, DataWrapper(2, new DataLeaf(3)))
      hbs(ctx).trim should equal("""Recursing on self: 1
                                        |Recursing on self: 2
                                        |Recursing on self: 3""".stripMargin)
    }

    it("not infinite loop with circular dependencies") {
      case class Person(firstName: String, age: Int)
      case class SomethingElse(person: Person)
      case class Intermediate(somethingElse: SomethingElse)
      case class Data(person: Person, intermediate: Intermediate)
      val hbs = Handlebars(new File("src/test/resources/person.handlebars"))
      val ctx = Data(Person("Chris", 12), Intermediate(SomethingElse(Person("Rich", 45))))
      hbs(ctx).trim should equal("""Chris -> 12
                                   |
                                   |
                                   |    Rich -> 45""".stripMargin)
    }
  }
}
