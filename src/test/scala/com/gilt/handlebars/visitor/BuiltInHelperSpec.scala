package com.gilt.handlebars.visitor

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.gilt.handlebars.Handlebars
import com.gilt.handlebars.context.Person
import com.gilt.handlebars.helper.{HelperContext, Helper}
import com.gilt.handlebars.parser.HandlebarsGrammar

/**
 * User: chicks
 * Date: 12/29/13
 */
class BuiltInHelperSpec extends FunSpec with ShouldMatchers {
  describe("build-in helpers") {
    it("with") {
      case class Person(first: String, last: String)
      case class Ctx(person: Person)
      val template = "{{#with person}}{{first}} {{last}}{{/with}}"
      val ctx = Ctx(Person("Alan", "Johnson"))
      Handlebars(template)(ctx) should equal("Alan Johnson")
    }

    it("with with function argument") {
      case class Person(first: String, last: String)
      trait Ctx {
        def person: Person
      }
      val template = "{{#with person}}{{first}} {{last}}{{/with}}"
      val ctx = new Ctx {
        def person: Person = Person("Alan", "Johnson")
      }
      Handlebars(template)(ctx) should equal("Alan Johnson")
    }

    it("if") {
      case class Goodbye(goodbye: Any, world: String)
      case class World(world: String)
      val template = "{{#if goodbye}}GOODBYE {{/if}}cruel {{world}}!"
      val hbs = Handlebars(template)

      hbs(Goodbye(true, "world")) should equal("GOODBYE cruel world!")
      hbs(Goodbye("dummy", "world")) should equal("GOODBYE cruel world!")
      hbs(Goodbye(false, "world")) should equal("cruel world!")
      hbs(World("world")) should equal("cruel world!")
      hbs(Goodbye(Iterable("foo"), "world")) should equal("GOODBYE cruel world!")
      hbs(Goodbye(Iterable.empty, "world")) should equal("cruel world!")
    }

    it("each") {
      case class Text(text: String)
      case class Ctx(goodbyes: Iterable[Text], world: String)
      val template = "{{#each goodbyes}}{{text}}! {{/each}}cruel {{world}}!"
      val hbs = Handlebars(template)

      hbs(Ctx(Iterable(Text("goodbye"), Text("Goodbye"), Text("GOODBYE")), "world")) should equal("goodbye! Goodbye! GOODBYE! cruel world!")
      hbs(Ctx(Iterable.empty, "world")) should equal("cruel world!")
    }

    it("each with an object and @key") {
      case class Text(text: String)
      case class Ctx(goodbyes: Map[Any, Text], world: String)
      val template = "{{#each goodbyes}}{{@key}}. {{text}}! {{/each}}cruel {{world}}!"
      val ctx = Ctx(Map("<b>#1</b>" -> Text("goodbye"), 2 -> Text("GOODBYE")), "world")
      Handlebars(template)(ctx) should equal("&lt;b&gt;#1&lt;/b&gt;. goodbye! 2. GOODBYE! cruel world!")
    }

    it("each with @index") {
      case class Text(text: String)
      case class Ctx(goodbyes: Iterable[Text], world: String)
      val template = "{{#each goodbyes}}{{@index}}. {{text}}! {{/each}}cruel {{world}}!"
      val ctx = Ctx(Iterable(Text("goodbye"), Text("Goodbye"), Text("GOODBYE")), "world")
      Handlebars(template)(ctx) should equal("0. goodbye! 1. Goodbye! 2. GOODBYE! cruel world!")
    }

    it("data passed to helpers") {
      case class Ctx(letters: List[String])
      val template = "{{#each letters}}{{this}}{{detectDataInsideEach}}{{/each}}"
      val ctx = Ctx(List("a", "b", "c"))
      val detectDataHelper = Helper {
        (context, options) =>
          options.getData("exclaim")
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("detectDataInsideEach" -> detectDataHelper))

      builder.build(ctx, Map("exclaim" -> "!")) should equal("a!b!c!")
    }

    it("log") {
      case class Ctx(blah: String)
      val template = "{{log blah}}"
      // Result should be "" as log doesn't print anything inside the template, but uses the slf4j logging from
      // Loggable.scala. There should be an 'INFO - whee' in the test output
      Handlebars(template)(Ctx("whee")) should equal("")
    }

    it("passing in data to a compiled function that expects data - works with helpers") {
      case class Ctx(noun: String)
      val template = "{{hello}}"
      val helloHelper = Helper {
        (context, options) =>
          val noun = context.lookup(List("noun"), List.empty).model
          "%s %s".format(options.getData("adjective"), noun)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(Ctx("cat"), Map("adjective" -> "happy")) should equal("happy cat")
    }

    it("data can be looked up via @foo") {
      val template = "{{@hello}}"
      Handlebars(template)(new {}, Map("hello" -> "hello")) should equal("hello")
    }

    it("deep @foo triggers automatic top-level data") {
      case class Ctx(foo: Boolean)
      val template = "{{#let world=\"world\"}}{{#if foo}}{{#if foo}}Hello {{@world}}{{/if}}{{/if}}{{/let}}"
      val letHelper = Helper {
        (context, options) =>
          options.visit(HelperContext(context, Map.empty))
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("let" -> letHelper))
      builder.build(Ctx(true)) should equal("Hello world")
    }

    it("parameter data can be looked up via @foo") {
      val template = "{{hello @world}}"
      val helloHelper = Helper {
        (context, options) =>
          "Hello %s".format(options.firstArgAsString)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> "world")) should equal("Hello world")
    }

    it("hash values can be looked up via @foo") {
      val template = "{{hello noun=@world}}"
      val helloHelper = Helper {
        (context, options) =>
          "Hello %s".format(options.getData("noun"))
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> "world")) should equal("Hello world")
    }

    it("nested parameter data can be looked up via @foo.bar") {
      case class Bar(bar: String)
      val template = "{{hello @world.bar}}"
      val helloHelper = Helper {
        (context, options) =>
          "Hello %s".format(options.firstArgAsString)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> Bar("world"))) should equal("Hello world")
    }

    it("nested parameter data does not fail with @world.bar") {
      case class Bar(bar: String)
      val template = "{{hello @world.bar}}"
      val helloHelper = Helper {
        (context, options) =>
          "Hello %s".format(options.firstArgAsString)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      // The JS version expects "Hello undefined" which is not a thing in scala. scandlebars will use "" instead
      builder.build(new {}, Map("foo" -> Bar("world"))) should equal("Hello ")
    }

    it("parameter data throws when using this scope references") {
      val template = "{{#goodbyes}}{{text}} cruel {{@./name}}! {{/goodbyes}}"
      intercept[RuntimeException] {
        Handlebars(template)
      }
    }

    it("parameter data throws when using parent scope references") {
      val template = "{{#goodbyes}}{{text}} cruel {{@../name}}! {{/goodbyes}}"
      intercept[RuntimeException] {
        Handlebars(template)
      }
    }

    it("parameter data throws when using complex scope references") {
      val template = "{{#goodbyes}}{{text}} cruel {{@foo/../name}}! {{/goodbyes}}"
      intercept[RuntimeException] {
        Handlebars(template)
      }
    }

    it("data is inherited downstream") {
      case class Baz(baz: String)
      case class Ctx(bar: Baz)
      val template = "{{#let foo=bar.baz}}{{@foo}}{{/let}}"
      val letHelper = Helper {
        (context, options) =>
          options.visit(HelperContext(context))
      }
      val ctx = Ctx(Baz("hello world"))
      val builder = Handlebars.createBuilder(template).withHelpers(Map("let" -> letHelper))
      builder.build(ctx) should equal("hello world")
    }
  }
}
