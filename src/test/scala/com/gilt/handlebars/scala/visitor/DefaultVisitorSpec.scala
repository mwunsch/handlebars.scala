package com.gilt.handlebars.scala.visitor

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.helper.Helper
import org.scalatest.{FunSpec, Matchers}

class DefaultVisitorSpec extends FunSpec with Matchers {

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

    // handlebars.scala doesn't support mustache escaping
    ignore("escaping") { }

    it("compiling with a basic context") {
      Handlebars("Goodbye\\n{{cruel}}\\n{{world}}!").apply(context) should equal("Goodbye\\ncruel\\nworld!")
    }

    it("comments") {
      Handlebars("{{! Goodbye}}Goodbye\\n{{cruel}}\\n{{world}}!").apply(context) should equal("Goodbye\\ncruel\\nworld!")
    }

    it("boolean") {
      Handlebars("{{#goodbye}}GOODBYE {{/goodbye}}cruel {{world}}!").apply(context) should equal("GOODBYE cruel world!") //
    }

    it("zeros") {
      val nestedCtx = new {
        val num1 = new {
          val num2 = 0
        }
      }

      Handlebars("num1: {{num1}}, num2: {{num2}}").apply(context) should equal("num1: 42, num2: 0")
      Handlebars("num: {{.}}").apply(0) should equal("num: 0")
      Handlebars("num: {{num1/num2}}").apply(nestedCtx) should equal ("num: 0")
    }

    it("newlines") {
      Handlebars("Alan's\nTest").apply("") should equal("Alan's\nTest")
      Handlebars("Alan's\rTest").apply("") should equal("Alan's\rTest")
    }

    it("escaping text") {
      Handlebars("Awesome {{foo}}").apply(Map("foo" -> "\\")) should equal("Awesome \\")
      Handlebars("Awesome's").apply("") should equal("Awesome's")
      Handlebars("Awesome\\").apply("") should equal("Awesome\\")
      Handlebars("Awesome\\\\ foo").apply("") should equal("Awesome\\\\ foo")
      Handlebars(""" " " """).apply("") should equal(""" " " """)
    }

    it("escaping expressions") {
      Handlebars("{{{awesome}}}").apply(Map("awesome" -> "&\"\\<>")) should equal("&\"\\<>")
      Handlebars("{{&awesome}}").apply(Map("awesome" -> "&\"\\<>")) should equal("&\"\\<>")

      // NOTE, the JS version escapes '` to &#x27;&#x60;
      Handlebars("{{awesome}}").apply(Map("awesome" -> "&\"'`\\<>")) should equal("&amp;&quot;'`\\&lt;&gt;")
      Handlebars("{{awesome}}").apply(Map("awesome" -> "Escaped, <b> looks like: &lt;b&gt;")) should equal("Escaped, &lt;b&gt; looks like: &amp;lt;b&amp;gt;")
    }

    it("functions") {
      val awesome1 = new {
        def awesome = "Awesome"
      }
      val awesome2 = new {
        val more = "More awesome"
        def awesome = more
      }
      Handlebars("{{awesome}}").apply(awesome1) should equal("Awesome")
      Handlebars("{{awesome}}").apply(awesome2) should equal("More awesome")
    }

    it("functions with context argument") {
      val context = new {
        val frank = "Frank"
        def awesome(ctx: String) = ctx
      }
      Handlebars("{{awesome frank}}").apply(context) should equal("Frank")
    }


    it("nested paths") {
      val ctx = new {
        val alan = new {
          val expression = "beautiful"
        }
      }
      Handlebars("Goodbye {{alan/expression}} world!").apply(ctx) should equal("Goodbye beautiful world!")
    }

    it("nested paths with empty string value") {
      val ctx = new {
        val alan = new {
          val expression = ""
        }
      }
      Handlebars("Goodbye {{alan/expression}} world!").apply(ctx) should equal("Goodbye  world!")
    }

    it("current context path ({{.}}) doesn't hit helpers") {
      val template = "test: {{.}}"
      val awesomeHelper = Helper[Any] {
        (context, options) =>
          "awesome"
      }
      val builder = Handlebars.createBuilder(template).withHelpers(Map("awesome" -> awesomeHelper))
      builder.build(()) should equal("test: ")
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
      Handlebars(template).apply(ctx) should equal("")
      Handlebars(template).apply(ctx2) should equal("")
    }

    it("this keyword in paths") {
      val template = "{{#goodbyes}}{{this}}{{/goodbyes}}"
      val ctx = new {
        val goodbyes = List("goodbye", "Goodbye", "GOODBYE")
      }
      Handlebars(template).apply(ctx) should equal("goodbyeGoodbyeGOODBYE")
    }

    /*
     * This throws a compile error in the JavaScript implementation because of the use of 'this' in {{text/this/foo}}.
     * However this works fine in scala since {{text/this/foo}} is effectively {{text/foo}} which can yield the
     * desired result.
     */
    it("this keyword nested inside path") {
      val template = "{{#hellos}}{{text/this/foo}}{{/hellos}}"
      Handlebars(template).apply(new { val test = "test" }) should equal("")
    }

    case class Hellos(hellos: List[HelloText])
    case class HelloText(text: String)

    it("this keyword in helpers") {
      val helpers = Map (
        "foo" -> Helper[Any] {
          (context, options) =>
            "bar %s".format(options.argument(0).render)
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
        "foo" -> Helper[Any] {
          (context, options) => {
            "bar %s".format(options.argument(0).render)
          }
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
    it("inverted sections with unset value") {
      val template = "{{#goodbyes}}{{this}}{{/goodbyes}}{{^goodbyes}}Right On!{{/goodbyes}}"
      val ctx = new {
        val notThere = ""
      }
      Handlebars(template).apply(ctx) should equal ("Right On!")
    }

    it("inverted section with false value") {
      val template = "{{#goodbyes}}{{this}}{{/goodbyes}}{{^goodbyes}}Right On!{{/goodbyes}}"
      val ctx = new {
        val goodbyes = false
      }
      Handlebars(template).apply(ctx) should equal ("Right On!")
    }

    it("inverted section with empty set") {
      val template = "{{#goodbyes}}{{this}}{{/goodbyes}}{{^goodbyes}}Right On!{{/goodbyes}}"
      val ctx = new {
        val goodbyes = List.empty
      }
      Handlebars(template).apply(ctx) should equal ("Right On!")
    }
  }

  describe("blocks") {
    case class Goodbye(text: String, url: String = "")

    it("array") {
      val template = "{{#goodbyes}}{{text}}! {{/goodbyes}}cruel {{world}}!"
      val ctx = new {
        val world = "world"
        val goodbyes = Iterable(Goodbye("goodbye"), Goodbye("Goodbye"), Goodbye("GOODBYE"))
      }
      Handlebars(template).apply(ctx) should equal("goodbye! Goodbye! GOODBYE! cruel world!")
    }

    it("array with @index") {
      val template = "{{#goodbyes}}{{@index}}. {{text}}! {{/goodbyes}}cruel {{world}}!"
      val ctx = new {
        val world = "world"
        val goodbyes = Iterable(Goodbye("goodbye"), Goodbye("Goodbye"), Goodbye("GOODBYE"))
      }
      Handlebars(template).apply(ctx) should equal("0. goodbye! 1. Goodbye! 2. GOODBYE! cruel world!")
    }

    it("empty block") {
      val template = "{{#goodbyes}}{{/goodbyes}}cruel {{world}}!"
      val ctx = new {
        val world = "world"
        val goodbyes = Iterable(Goodbye("goodbye"), Goodbye("Goodbye"), Goodbye("GOODBYE"))
      }
      Handlebars(template).apply(ctx) should equal("cruel world!")
    }

    it("empty block 2") {
      val template = "{{#goodbyes}}{{/goodbyes}}cruel {{world}}!"
      val ctx = new {
        val world = "world"
        val goodbyes = Iterable.empty
      }
      Handlebars(template).apply(ctx) should equal("cruel world!")
    }

    it("block with complex lookup") {
      val template = "{{#goodbyes}}{{text}} cruel {{../name}}! {{/goodbyes}}"
      val ctx = new {
        val name = "Alan"
        val goodbyes = Iterable(Goodbye("goodbye"), Goodbye("Goodbye"), Goodbye("GOODBYE"))
      }
      Handlebars(template).apply(ctx) should equal("goodbye cruel Alan! Goodbye cruel Alan! GOODBYE cruel Alan! ")
    }

    /*
     * In JavasScript implementation the template below should not compile. handlebars.scala simply renders the invalid
     * path, {{foo/../name}}, as an empty string.
     */
    it("block with complex lookup using nested context") {
      val template = "{{#goodbyes}}{{text}} cruel {{foo/../name}}! {{/goodbyes}}"
      val ctx = new {
        val name = "Alan"
        val goodbyes = Iterable(Goodbye("goodbye"), Goodbye("Goodbye"), Goodbye("GOODBYE"))
      }
      Handlebars(template).apply(ctx) should equal("goodbye cruel ! Goodbye cruel ! GOODBYE cruel ! ")
    }

    it("helper with complex lookup$") {
      val template = "{{#goodbyes}}{{{link ../prefix}}}{{/goodbyes}}"
      val ctx = new {
        val prefix = "/root"
        val goodbyes = Iterable(Goodbye("Goodbye", "goodbye"))
      }
      val helpers = Map(
        "link" -> Helper[Any] {
          (goodbye, options) =>
            val obj = goodbye.get.asInstanceOf[Goodbye]
            """<a href="%s/%s">%s</a>""".format(options.argument(0).get, obj.url, obj.text)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<a href=\"/root/goodbye\">Goodbye</a>")
    }

    it("helper block with complex lookup expression") {
      case class Alan(name: String)
      val template = "{{#goodbyes}}{{../name}}{{/goodbyes}}"
      val ctx = Alan("Alan")
      val helpers = Map(
        "goodbyes" -> Helper[Any] {
          (context, options) =>
            val byes = List("goodbye", "Goodbye", "GOODBYE")
            byes.map(bye => "%s %s! ".format(bye, options.visit(context))).mkString
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("goodbye Alan! Goodbye Alan! GOODBYE Alan! ")
    }

    it("helper with complex lookup and nested template") {
      val template = "{{#goodbyes}}{{#link ../prefix}}{{text}}{{/link}}{{/goodbyes}}"
      val ctx = new {
        val prefix = "/root"
        val goodbyes = Iterable(Goodbye("Goodbye", "goodbye"))
      }
      val helpers = Map(
        "link" -> Helper[Any] {
          (goodbye, options) =>
            val obj = goodbye.get.asInstanceOf[Goodbye]
            """<a href="%s/%s">%s</a>""".format(options.argument(0).get, obj.url, obj.text)
        }
      )
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<a href=\"/root/goodbye\">Goodbye</a>")
    }

    it("block with deep nested complex lookup") {
      case class Hash(omg: String, outer: Iterable[Inner])
      case class Inner(inner: Iterable[Goodbye])
      val template = "{{#outer}}Goodbye {{#inner}}cruel {{../../omg}}{{/inner}}{{/outer}}"
      val ctx = Hash("OMG!", Iterable(Inner(Iterable(Goodbye("goodbye")))))

      Handlebars(template).apply(ctx) should equal("Goodbye cruel OMG!")
    }

    it("block helper") {
      case class Text(text: String)
      val template = "{{#goodbyes}}{{text}}! {{/goodbyes}}cruel {{world}}!"
      val helpers = Map(
        "goodbyes" -> Helper[Any] {
          (context, options) =>
            options.visit(Text("GOODBYE"))
        }
      )
      val ctx = new {
        val world = "world"
      }
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("GOODBYE! cruel world!")
    }

    it("block helper staying in the same context") {
      val template = "{{#form}}<p>{{name}}</p>{{/form}}"
      val helpers = Map(
        "form" -> Helper[Any] {
          (context, options) =>
            "<form>%s</form>".format(options.visit(context))
        }
      )
      val ctx = new {
        val name = "Yehuda"
      }
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<form><p>Yehuda</p></form>")
    }

    it("block helper should have context in this") {
      case class Person(name: String, id: Int)
      case class People(people: List[Person])

      val template = "<ul>{{#people}}<li>{{#link}}{{name}}{{/link}}</li>{{/people}}</ul>"
      val helpers = Map(
        "link" -> Helper[Any] {
          (person, options) =>
            val self = person.get.asInstanceOf[Person]
            "<a href=\"/people/%s\">%s</a>".format(self.id, options.visit(person))
        }
      )
      val ctx = People(List(Person("Alan", 1), Person("Yehuda", 2)))

      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<ul><li><a href=\"/people/1\">Alan</a></li><li><a href=\"/people/2\">Yehuda</a></li></ul>")
    }

    it("block helper for undefined value") {
      val template = "{{#empty}}shouldn't render{{/empty}}"
      val ctx = new { val notUsed = "notUsed" }
      Handlebars(template).apply(ctx) should equal("")
    }

    it("block helper passing a new context") {
      case class Person(name: String)
      case class Yehuda(yehuda: Person)

      val template = "{{#form yehuda}}<p>{{name}}</p>{{/form}}"
      val helpers = Map(
        "form" -> Helper[Any] {
          (context, options) => {
            "<form>%s</form>".format(options.visit(options.argument(0).get))
          }
        }
      )
      val ctx = Yehuda(Person("Yehuda"))

      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<form><p>Yehuda</p></form>")
    }

    it("block helper passing a complex path context") {
      case class Yehuda(yehuda: Person)
      case class Person(name: String, cat: Cat)
      case class Cat(name: String)

      val template = "{{#form yehuda/cat}}<p>{{name}}</p>{{/form}}"
      val helpers = Map (
        "form" -> Helper[Any] {
          (context, options) =>
            "<form>%s</form>".format(options.visit(options.argument(0).get))
        }
      )
      val ctx = Yehuda(Person("Yehuda", Cat("Harold")))
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<form><p>Harold</p></form>")
    }

    it("nested block helpers") {
      case class Yehuda(yehuda: Person)
      case class Person(name: String)

      val template = "{{#form yehuda}}<p>{{name}}</p>{{#link}}Hello{{/link}}{{/form}}"
      val helpers = Map (
        "link" -> Helper[Any] {
          (person, options) =>
            val yehuda = person.get.asInstanceOf[Person]
            "<a href='%s'>%s</a>".format(yehuda.name, options.visit(person))
        },
        "form" -> Helper[Any] {
          (context, options) =>
            "<form>%s</form>".format(options.visit(options.argument(0).get))
        }
      )
      val ctx = Yehuda(Person("Yehuda"))
      val builder = Handlebars.createBuilder(template).withHelpers(helpers).build
      builder(ctx) should equal("<form><p>Yehuda</p><a href='Yehuda'>Hello</a></form>")
    }

    it("block inverted sections") {
      case class NoPeople(none: String)
      val template = "{{#people}}{{name}}{{^}}{{none}}{{/people}}"
      val ctx = NoPeople("No People")
      Handlebars(template).apply(ctx) should equal("No People")
    }

    it("block inverted sections with empty arrays") {
      case class NoPeople(none: String, people: List[Person])
      case class Person(name: String)

      val template = "{{#people}}{{name}}{{^}}{{none}}{{/people}}"
      val ctx = NoPeople("No People", List.empty)
      Handlebars(template).apply(ctx) should equal("No People")
    }

    it("block helper inverted sections") {
      case class People(people: List[Person])
      case class Person(name: String)
      case class RootMessage(people: List[Person], message: String)

      val string = "{{#list people}}{{name}}{{^}}<em>Nobody's here</em>{{/list}}"
      val helpers = Map (
        "list" -> Helper[Any] {
          (context, options) =>
            options.argument(0).get match {
              case i:Iterable[Any] =>
                if (i.isEmpty) {
                  "<p>%s</p>".format(options.inverse(context))
                } else {
                  val listItems = i.map(item => "<li>%s</li>".format(options.visit(item)))
                  "<ul>%s</ul>".format(listItems.mkString)
                }
              case _ =>
                options.inverse(context)
            }
        }
      )
      val hash = People(List(Person("Alan"), Person("Yehuda")))
      val empty = People(List.empty)
      val rootMessage = RootMessage(List.empty, "Nobody's here")

      val messageString = "{{#list people}}Hello{{^}}{{message}}{{/list}}"

      Handlebars.createBuilder(string)
                .withHelpers(helpers)
                .build(hash) should equal("<ul><li>Alan</li><li>Yehuda</li></ul>")

      Handlebars.createBuilder(string)
        .withHelpers(helpers)
        .build(empty) should equal("<p><em>Nobody's here</em></p>")

      Handlebars.createBuilder(messageString)
        .withHelpers(helpers)
        .build(rootMessage) should equal("<p>Nobody's here</p>")
    }
  }
}
