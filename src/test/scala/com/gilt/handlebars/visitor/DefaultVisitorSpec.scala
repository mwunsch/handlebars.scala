package com.gilt.handlebars.visitor

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.gilt.handlebars.Handlebars

/**
 * User: chicks
 * Date: 12/1/13
 */
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
  }

}
