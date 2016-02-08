package com.gilt.handlebars.scala.visitor

import com.gilt.handlebars.scala.Handlebars
import org.scalatest.{ FunSpec, Matchers }
import com.gilt.handlebars.scala.helper.Helper
import com.gilt.handlebars.scala.binding.dynamic._

/**
 * User: chicks
 * Date: 12/29/13
 */
class BuiltInHelperSpec extends FunSpec with Matchers {
  describe("build-in helpers") {
    it("with") {
      case class Person(first: String, last: String)
      case class Ctx(person: Person)
      val template = "{{#with person}}{{first}} {{last}}{{/with}}"
      val ctx = Ctx(Person("Alan", "Johnson"))
      Handlebars(template).apply(ctx) should equal("Alan Johnson")
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
      Handlebars(template).apply(ctx) should equal("Alan Johnson")
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

    it("unless") {
      case class Hello(goodbye: Any, world: String)
      case class World(world: String)
      val template = "{{#unless goodbye}}HELLO {{else}}GOODBYE cruel {{/unless}}{{world}}!"
      val hbs = Handlebars(template)

      hbs(Hello(true, "world")) should equal("GOODBYE cruel world!")
      hbs(Hello("dummy", "world")) should equal("GOODBYE cruel world!")
      hbs(Hello(false, "world")) should equal("HELLO world!")
      hbs(Hello(Iterable("foo"), "world")) should equal("GOODBYE cruel world!")
      hbs(Hello(Iterable.empty, "world")) should equal("HELLO world!")
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
      Handlebars(template).apply(ctx) should equal("&lt;b&gt;#1&lt;/b&gt;. goodbye! 2. GOODBYE! cruel world!")
    }

    it("each with @index") {
      case class Text(text: String)
      case class Ctx(goodbyes: Iterable[Text], world: String)
      val template = "{{#each goodbyes}}{{@index}}. {{text}}! {{/each}}cruel {{world}}!"
      val ctx = Ctx(Iterable(Text("goodbye"), Text("Goodbye"), Text("GOODBYE")), "world")
      Handlebars(template).apply(ctx) should equal("0. goodbye! 1. Goodbye! 2. GOODBYE! cruel world!")
    }

    it("data passed to helpers") {
      case class Ctx(letters: List[String])
      val template = "{{#each letters}}{{this}}{{detectDataInsideEach}}{{/each}}"
      val ctx = Ctx(List("a", "b", "c"))
      val detectDataHelper = Helper[Any] {
        (context, options) =>
          options.data("exclaim").render
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("detectDataInsideEach" -> detectDataHelper))

      builder.build(ctx, Map("exclaim" -> "!")) should equal("a!b!c!")
    }

    it("log") {
      case class Ctx(blah: String)
      val template = "{{log blah}}"
      // Result should be "" as log doesn't print anything inside the template, but uses the slf4j logging from
      // Loggable.scala. There should be an 'INFO - whee' in the test output
      Handlebars(template).apply(Ctx("whee")) should equal("")
    }

    it("passing in data to a compiled function that expects data - works with helpers") {
      case class Ctx(noun: String)
      val template = "{{hello}}"
      val helloHelper = Helper[Any] {
        (context, options) =>
          val noun = options.lookup("noun").get
          "%s %s".format(options.data("adjective").get, noun)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(Ctx("cat"), Map("adjective" -> "happy")) should equal("happy cat")
    }

    it("data can be looked up via @foo") {
      val template = "{{@hello}}"
      Handlebars(template).apply(Map.empty, Map("hello" -> "hello")) should equal("hello")
    }

    it("deep @foo triggers automatic top-level data") {
      case class Ctx(foo: Boolean)
      val template = "{{#let world=\"world\"}}{{#if foo}}{{#if foo}}Hello {{@world}}{{/if}}{{/if}}{{/let}}"
      val letHelper = Helper[Any] {
        (context, options) =>
          options.visit(context, Map.empty)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("let" -> letHelper))
      builder.build(Ctx(true)) should equal("Hello world")
    }

    it("parameter data can be looked up via @foo") {
      val template = "{{hello @world}}"
      val helloHelper = Helper[Any] { (context, options) =>
        "Hello %s".format(options.argument(0).get)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> "world")) should equal("Hello world")
    }

    it("hash values can be looked up via @foo") {
      val template = "{{hello noun=@world}}"
      val helloHelper = Helper[Any] {
        (context, options) => {
          "Hello %s".format(options.data("noun").get)
        }
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> "world")) should equal("Hello world")
    }

    it("nested parameter data can be looked up via @foo.bar") {
      case class Bar(bar: String)
      val template = "{{hello @world.bar}}"
      val helloHelper = Helper[Any] {
        (context, options) =>
          "Hello %s".format(options.argument(0).get)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(new {}, Map("world" -> Bar("world"))) should equal("Hello world")
    }

    it("nested parameter data does not fail with @world.bar") {
      case class Bar(bar: String)
      val template = "{{hello @world.bar}}"
      val helloHelper = Helper[Any] {
        (context, options) =>
          "Hello %s".format(options.argument(0).render)
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      // The JS version expects "Hello undefined" which is not a thing in scala. scandlebars will use "" instead
      builder.build(Map.empty, Map("foo" -> Bar("world"))) should equal("Hello ")
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
      val letHelper = Helper[Any] {
        (context, options) =>
          options.visit(context)
      }
      val ctx = Ctx(Baz("hello world"))
      val builder = Handlebars.createBuilder(template).withHelpers(Map("let" -> letHelper))
      builder.build(ctx) should equal("hello world")
    }

    // TODO: Come back to this when partials are implemented
    ignore("passing in data to a compiled function that expects data - works with helpers in partials") {

    }

    it("passing in data to a compiled function that expects data - works with helpers and parameters") {
      case class Ctx(exclaim: Boolean, world: String)
      val template = "{{hello world}}"
      val helloHelper = Helper[Any] {
        (context, options) =>
          val exclaim = options.lookup("exclaim").get.asInstanceOf[Boolean]
          "%s %s%s".format(options.data("adjective").get, options.argument(0).get, if (exclaim) "!" else "")
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("hello" -> helloHelper))
      builder.build(Ctx(true, "world"), Map("adjective" -> "happy")) should equal("happy world!")
    }

    it("passing in data to a compiled function that expects data - works with block helpers") {
      case class Ctx(exclaim: Boolean)
      val template = "{{#hello}}{{world}}{{/hello}}"
      val helpers = Map (
        "hello" -> Helper[Any] {
          (context, options) =>
            options.visit(context)
        },
        "world" -> Helper[Any] {
          (context, options) =>
            val exclaim = options.lookup("exclaim").get.asInstanceOf[Boolean]
            "%s world%s".format(options.data("adjective").get, if (exclaim) "!" else "")
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx(true), Map("adjective" -> "happy")) should equal("happy world!")
    }

    it("passing in data to a compiled function that expects data - works with block helpers that use ..") {
      case class Ctx(exclaim: Boolean, zomg: String)
      case class Exclaim(exclaim: String)
      val template = "{{#hello}}{{world ../zomg}}{{/hello}}"
      val helpers = Map (
        "hello" -> Helper[Any] {
          (context, options) =>
            options.visit(Exclaim("?"))
        },
        "world" -> Helper[Any] {
          (context, options) =>
            val exclaim = options.lookup("exclaim").render
            "%s %s%s".format(options.data("adjective").get, options.argument(0).get, exclaim)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx(true, "world"), Map("adjective" -> "happy")) should equal("happy world?")
    }

    it("passing in data to a compiled function that expects data - data is passed to with block helpers where children use ..") {
      case class Ctx(exclaim: Boolean, zomg: String)
      case class Exclaim(exclaim: String)
      val template = "{{#hello}}{{world ../zomg}}{{/hello}}"
      val helpers = Map (
        "hello" -> Helper[Any] {
          (context, options) =>
            "%s %s".format(options.data("accessData").get, options.visit(Exclaim("?")))
        },
        "world" -> Helper[Any] {
          (context, options) =>
            val exclaim = options.lookup("exclaim").render
            "%s %s%s".format(options.data("adjective").get, options.argument(0).get, exclaim)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx(true, "world"), Map("adjective" -> "happy", "accessData" -> "#win")) should equal("#win happy world?")
    }

    it("you can override inherited data when invoking a helper") {
      case class Ctx1(exclaim: String, zomg: String)
      case class Ctx2(exclaim: Boolean, zomg: String)
      val template = "{{#hello}}{{world zomg}}{{/hello}}"
      val helpers = Map (
        "hello" -> Helper[Any] {
          (context, options) =>
            options.visit(Ctx1("?", "world"), Map("adjective" -> "sad"))
        },
        "world" -> Helper[Any] {
          (context, options) =>
            val exclaim = options.lookup("exclaim").render
            "%s %s%s".format(options.data("adjective").get, options.argument(0).get, exclaim)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx2(true, "planet"), Map("adjective" -> "happy")) should equal("sad world?")
    }

    it("you can override inherited data when invoking a helper with depth") {
      case class Exclaim(exclaim: String)
      case class Ctx(exclaim: Boolean, zomg: String)
      val template = "{{#hello}}{{world ../zomg}}{{/hello}}"
      val helpers = Map (
        "hello" -> Helper[Any] {
          (context, options) =>
            options.visit(Exclaim("?"), Map("adjective" -> "sad"))
        },
        "world" -> Helper[Any] {
          (context, options) =>
            val exclaim = options.lookup("exclaim").render
            "%s %s%s".format(options.data("adjective").get, options.argument(0).get, exclaim)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx(true, "world"), Map("adjective" -> "happy")) should equal("sad world?")
    }

    it("helpers take precedence over same-named context properties") {
      case class Ctx(goodbye: String, world: String)
      val template = "{{goodbye}} {{cruel world}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            val goodbye = options.lookup("goodbye").render
            goodbye.toString.toUpperCase
        },
        "cruel" -> Helper[Any] {
          (context, options) =>
            val world = options.lookup("world").render
            "cruel %s".format(world.toString.toUpperCase)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("goodbye", "world")) should equal("GOODBYE cruel WORLD")
    }

    it("helpers take precedence over same-named context properties$") {
      case class Ctx(goodbye: String, world: String)
      val template = "{{#goodbye}} {{cruel world}}{{/goodbye}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            val goodbye = options.lookup("goodbye").render
            "%s%s".format(goodbye.toString.toUpperCase, options.visit(context))
        },
        "cruel" -> Helper[Any] {
          (context, options) =>
            val world = options.lookup("world").render
            "cruel %s".format(world.toString.toUpperCase)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("goodbye", "world")) should equal("GOODBYE cruel WORLD")
    }

    it("Scoped names take precedence over helpers") {
      case class Ctx(goodbye: String, world: String)
      val template = "{{this.goodbye}} {{cruel world}} {{cruel this.goodbye}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            val goodbye = options.lookup("goodbye").render
            goodbye.toString.toUpperCase
        },
        "cruel" -> Helper[Any] {
          (context, options) =>
            "cruel %s".format(options.argument(0).get.toString.toUpperCase)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("goodbye", "world")) should equal("goodbye cruel WORLD cruel GOODBYE")
    }

    it("Scoped names take precedence over block helpers") {
      case class Ctx(goodbye: String, world: String)
      val template = "{{#goodbye}} {{cruel world}}{{/goodbye}} {{this.goodbye}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            val goodbye = options.lookup("goodbye").render
            "%s%s".format(goodbye.toString.toUpperCase, options.visit(context))
        },
        "cruel" -> Helper[Any] {
          (context, options) =>
            "cruel %s".format(options.argument(0).get.toString.toUpperCase)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("goodbye", "world")) should equal("GOODBYE cruel WORLD goodbye")
    }

    it("helpers can take a nested helper") {
      case class Ctx(worldKey: String)
      val template = "goodbye {{cruel ( world ) }}"
      val helpers = Map (
        "cruel" -> Helper[Any] {
          (context, options) =>
            "cruel %s".format(options.argument(0).get.toString.toUpperCase)
        },

        "world" -> Helper[Any] {
          (context, options) =>
            options.lookup("worldKey").render + "!"
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("world")) should equal("goodbye cruel WORLD!")
    }

    it("helpers can take a nested helper within nested helper") {
      case class Ctx(worldKey: String)
      val template = "goodbye {{cruel ( cruel (world) ) }}"
      val helpers = Map (
        "cruel" -> Helper[Any] {
          (context, options) =>
            "cruel %s".format(options.argument(0).get.toString.toUpperCase)
        },

        "world" -> Helper[Any] {
          (context, options) =>
            options.lookup("worldKey").render + "!"
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(Ctx("world")) should equal("goodbye cruel CRUEL WORLD!")
    }

    it("helpers can take an optional hash") {
      val template = "{{goodbye cruel=\"CRUEL\" world=\"WORLD\" times=12}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            "GOODBYE %s %s %s TIMES".format(options.data("cruel").get, options.data("world").get, options.data("times").get)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(new {}) should equal("GOODBYE CRUEL WORLD 12 TIMES")
    }

    it("helpers can take an optional hash with booleans") {
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            val print = options.data("print")
            if (print.isTruthy) {
              "GOODBYE %s %s".format(options.data("cruel").get, options.data("world").get)
            } else {
              "NOT PRINTING"
            }
        }
      )
      val template1 = "{{goodbye cruel=\"CRUEL\" world=\"WORLD\" print=true}}"
      val builder1 = Handlebars.createBuilder(template1).withHelpers(helpers)
      builder1.build(new {}) should equal("GOODBYE CRUEL WORLD")

      val template2 = "{{goodbye cruel=\"CRUEL\" world=\"WORLD\" print=false}}"
      val builder2 = Handlebars.createBuilder(template2).withHelpers(helpers)
      builder2.build(new {}) should equal("NOT PRINTING")
    }

    it("block helpers can take an optional hash") {
      val template = "{{#goodbye cruel=\"CRUEL\" times=12}}world{{/goodbye}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            "GOODBYE %s %s %s TIMES".format(options.data("cruel").get, options.visit(context), options.data("times").get)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(new {}) should equal("GOODBYE CRUEL world 12 TIMES")
    }

    ignore("block helpers can take an optional hash with single quoted strings") {
      // handlebars.scala does not support single quote strings for hashes
      val template = "{{#goodbye cruel='CRUEL' times=12}}world{{/goodbye}}"
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            "GOODBYE %s %s %s TIMES".format(options.data("cruel").render, options.visit(context), options.data("times"))
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(new {}) should equal("GOODBYE CRUEL world 12 TIMES")
    }

    it("block helpers can take an optional hash with booleans") {
      val helpers = Map (
        "goodbye" -> Helper[Any] {
          (context, options) =>
            if (options.data("print").isTruthy) {
              "GOODBYE %s %s".format(options.data("cruel").get, options.visit(context))
            } else {
              "NOT PRINTING"
            }
        }
      )
      val template1 = "{{#goodbye cruel=\"CRUEL\" print=true}}world{{/goodbye}}"
      val builder1 = Handlebars.createBuilder(template1).withHelpers(helpers)
      builder1.build(new {}) should equal("GOODBYE CRUEL world")

      val template2 = "{{#goodbye cruel=\"CRUEL\" print=false}}world{{/goodbye}}"
      val builder2 = Handlebars.createBuilder(template2).withHelpers(helpers)
      builder2.build(new {}) should equal("NOT PRINTING")
    }

    ignore("arguments to helpers can be retrieved from options hash in string form") {
      // handlebars.scala does not support string parameter mode
      val template = "{{wycats is.a slave.driver}}"
      val helpers = Map (
        "wycats" -> Helper[Any] {
          (context, options) =>
            "HELP ME MY BOSS %s %s".format(options.argument(0).get, options.argument(1).get)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers)
      builder.build(new {}) should equal("HELP ME MY BOSS is.a slave.driver")
    }

    ignore("when using block form, arguments to helpers can be retrieved from options hash in string form") {
      // handlebars.scala does not support string parameter mode
    }

    ignore("when inside a block in String mode, .. passes the appropriate context in the options hash") {
      // handlebars.scala does not support string parameter mode
    }

    ignore("in string mode, information about the types is passed along") {
      // handlebars.scala does not support string parameter mode
    }

    ignore("in string mode, hash parameters get type information") {
      // handlebars.scala does not support string parameter mode
    }

    ignore("when inside a block in String mode, .. passes the appropriate context in the options hash to a block helper") {
      // handlebars.scala does not support string parameter mode
    }
  }
}
