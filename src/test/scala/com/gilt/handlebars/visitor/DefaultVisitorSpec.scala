package com.gilt.handlebars.visitor

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.gilt.handlebars.Handlebars
import com.gilt.handlebars.helper.Helper

class DefaultVisitorSpec extends FunSpec with ShouldMatchers {

  describe("basic context") {
    val context = new {
      val foo = "foo"
      val cruel = "cruel"
      val world = "world"
      val goodbye = true
      val num1 = 42
      val num2 = 0
    }

    it("most basic") {
      val template = "{{foo}}"
      val hbst = Handlebars(template)

      hbst(context) should equal("foo")
    }

    // TODO: build escaping into grammar if possible
//    it("escaping") {
//      println("program: \n\n%s".format(Handlebars("\\{{foo}}").program))
//      println("program: \n\n%s".format(Handlebars("\\\\{{foo}}").program))
//      println("program: \n\n%s".format(Handlebars("\\\\ {{foo}}").program))
//
//      Handlebars("\\{{foo}}")(context) should equal("{{foo}}")
//      Handlebars("\\\\{{foo}}")(context) should equal("\\foo")
//      Handlebars("\\\\ {{foo}}")(context) should equal("\\\\ foo")
//    }
    it("compiling with a basic context") {
      Handlebars("Goodbye\\n{{cruel}}\\n{{world}}!")(context) should equal("Goodbye\\ncruel\\nworld!")
    }

    it("comments") {
      Handlebars("{{! Goodbye}}Goodbye\\n{{cruel}}\\n{{world}}!")(context) should equal("Goodbye\\ncruel\\nworld!")
    }

    it("boolean") {
      Handlebars("{{#goodbye}}GOODBYE {{/goodbye}}cruel {{world}}!")(context) should equal("GOODBYE cruel world!")
    }

    it("zeros") {
      val nestedCtx = new {
        val num1 = new {
          val num2 = 0
        }
      }

      Handlebars("num1: {{num1}}, num2: {{num2}}")(context) should equal("num1: 42, num2: 0")
      Handlebars("num: {{.}}")(0) should equal("num: 0")
      Handlebars("num: {{num1/num2}}")(nestedCtx) should equal ("num: 0")
    }

    it("newlines") {
      Handlebars("Alan's\nTest")("") should equal("Alan's\nTest")
      Handlebars("Alan's\rTest")("") should equal("Alan's\rTest")
    }

    it("escaping text") {
      Handlebars("Awesome's")("") should equal("Awesome's")
      Handlebars("Awesome\\")("") should equal("Awesome\\")
      Handlebars("Awesome\\\\ foo")("") should equal("Awesome\\\\ foo")
      Handlebars("Awesome {{foo}}")(new { val foo = "\\"}) should equal("Awesome \\")
      Handlebars(""" " " """)("") should equal(""" " " """)
    }

    it("escaping expressions") {
      Handlebars("{{{awesome}}}")(new { val awesome = "&\"\\<>"}) should equal("&\"\\<>")
      Handlebars("{{&awesome}}")(new { val awesome = "&\"\\<>"}) should equal("&\"\\<>")

      // NOTE, the JS version escapes '` to &#x27;&#x60;
      Handlebars("{{awesome}}")(new { val awesome = "&\"'`\\<>"}) should equal("&amp;&quot;'`\\&lt;&gt;")
      Handlebars("{{awesome}}")(new { val awesome = "Escaped, <b> looks like: &lt;b&gt;"}) should equal("Escaped, &lt;b&gt; looks like: &amp;lt;b&amp;gt;")
    }

    it("functions") {
      val awesome1 = new {
        def awesome = "Awesome"
      }
      val awesome2 = new {
        val more = "More awesome"
        def awesome = more
      }
      Handlebars("{{awesome}}")(awesome1) should equal("Awesome")
      Handlebars("{{awesome}}")(awesome2) should equal("More awesome")
    }

    it("functions with context argument") {
      val context = new {
        val frank = "Frank"
        def awesome(ctx: String) = ctx
      }
      Handlebars("{{awesome frank}}")(context) should equal("Frank")
    }


    it("nested paths") {
      val ctx = new {
        val alan = new {
          val expression = "beautiful"
        }
      }
      Handlebars("Goodbye {{alan/expression}} world!")(ctx) should equal("Goodbye beautiful world!")
    }

    it("nested paths with empty string value") {
      val ctx = new {
        val alan = new {
          val expression = ""
        }
      }
      Handlebars("Goodbye {{alan/expression}} world!")(ctx) should equal("Goodbye  world!")
    }

    it("current context path ({{.}}) doesn't hit helpers") {
      val template = "test: {{.}}"
      val awesomeHelper = Helper {
        (context, args) =>
          "awesome"
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("awesome" -> awesomeHelper))
      builder.build(null) should equal("test: ")
    }

    it("complex but empty paths") {
      val ctx = new {
        val person = new {
          val name = null
        }
      }
      val ctx2 = new {
        val person = new {
          val notName = "notName"
        }
      }

      val template = "{{person/name}}"
      Handlebars(template)(ctx) should equal("")
      Handlebars(template)(ctx2) should equal("")
    }

    it("this keyword in paths") {
      val template = "{{#goodbyes}}{{this}}{{/goodbyes}}"
      val ctx = new {
        val goodbyes = List("goodbye", "Goodbye", "GOODBYE")
      }
      Handlebars(template)(ctx) should equal("goodbyeGoodbyeGOODBYE")
    }

    /*
     * This throws a compile error in the JavaScript implementation because of the use of 'this' in {{text/this/foo}}.
     * However this works fine in scala since {{text/this/foo}} is effectively {{text/foo}} which can yield the
     * desired result.
     */
    it("this keyword nested inside path") {
      val template = "{{#hellos}}{{text/this/foo}}{{/hellos}}"
      Handlebars(template)(new { val test = "test" }) should equal("")
    }

    case class Hellos(hellos: List[HelloText])
    case class HelloText(text: String)

    it("this keyword in helpers") {
      val helpers = Map (
        "foo" -> Helper {
          (context, args) =>
            "bar %s".format(args.toList(0))
        }
      )

      val template = "{{#goodbyes}}{{foo this}}{{/goodbyes}}"
      val ctx = new {
        val goodbyes = List("goodbye", "Goodbye", "GOODBYE")
      }
      val hbs = Handlebars.createBuilder(template).withHelpers(helpers).build

      hbs(ctx) should equal("bar goodbyebar Goodbyebar GOODBYE")

      val template2 = "{{#hellos}}{{foo this/text}}{{/hellos}}"
      val ctx2 = Hellos(List (
          HelloText("hello"),
          HelloText("Hello"),
          HelloText("HELLO")
        )
      )
      val hbs2 = Handlebars.createBuilder(template2).withHelpers(helpers).build

      hbs2(ctx2) should equal("bar hellobar Hellobar HELLO")
    }

    /*
     * This throws a compile error in the JavaScript implementation because of the use of 'this' in {{foo text/this/foo}}.
     * However this works fine in scala since {{foo text/this/foo}} is effectively {{foo text/foo}} which can yield the
     * desired result.
     */
    it("this keyword nested inside helpers param") {
      val helpers = Map (
        "foo" -> Helper {
          (context, args) =>
            "bar %s".format(args.toList(0))
        }
      )
      val template = "{{#hellos}}{{foo text/this/foo}}{{/hellos}}"
      val ctx = Hellos(List (
        HelloText("hello"),
        HelloText("Hello"),
        HelloText("HELLO")
      )
      )
      val hbs = Handlebars.createBuilder(template).withHelpers(helpers).build

      // text.foo doesn't exist so the first argument for the helper is ""
      hbs(ctx) should equal("bar bar bar ")
    }
  }

  describe("inverted sections") {

  }
}
