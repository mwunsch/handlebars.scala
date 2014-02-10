package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._
import com.google.common.base.Optional
import com.gilt.handlebars.Handlebars.Helper

class HandlebarsVisitorSpec extends Specification {
  "A Handlebars Visitor" should {

    "visit a program with only content" in {
      val program = Handlebars.parse("bar")
      val visitor = HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must beEqualTo("bar")
    }

    "visit a program with a simple mustache" in {
      val program = Handlebars.parse("{{head}} == foo")
      val visitor = HandlebarsVisitor(Seq("foo","bar","baz"))
      visitor.visit(program) must beEqualTo("foo == foo")
    }

    "silently fail with an empty string when a mustache can not be resolved" in {
      val program = Handlebars.parse("{{foo}} is empty")
      val visitor = HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must beEqualTo("is empty").ignoreSpace
    }

    "visit a program with the mustache containg a path: {{foo/bar}}" in {
      val program = Handlebars.parse("{{greeting/toUpperCase}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "Hello"
      })
      visitor.visit(program) must beEqualTo("HELLO, world.")
    }

    "visit a program with the mustache containg a path: {{foo.bar}}" in {
      val program = Handlebars.parse("{{greeting.toUpperCase}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "Hello"
      })
      visitor.visit(program) must beEqualTo("HELLO, world.")
    }

    "visit a program with the mustache containg a path: {{foo/../bar}}" in {
      val program = Handlebars.parse("{{greeting/../farewell}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "Hello"
        val farewell = "Goodbye"
      })
      visitor.visit(program) must beEqualTo("Goodbye, world.")
    }

    "visit a program with the mustache containg a path: {{foo/bar/../baz}}" in {
      val program = Handlebars.parse("{{greeting/hi/../yo}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = new {
          val hi = "Hi"
          val yo = "Yo"
          override def toString = "Hello"
        }
      })
      visitor.visit(program) must beEqualTo("Yo, world.")
    }

    "visit a program and remove comments" in {
      val program = Handlebars.parse("Hello,{{! cruel }} world.")
      val visitor = HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must not contain("cruel")
    }

    "visit a program and compile partials on the fly" in {
      val program = Handlebars.parse("{{> greeting }} world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "{{excited}}! Hi"
        val excited = "LOLOMG"
      })
      visitor.visit(program) must contain("LOLOMG")
    }

    "visit a program and use Map keys when the context is a map" in {
      val program = Handlebars.parse("{{greeting}}, world")
      val visitor = HandlebarsVisitor(Map("greeting" -> "Hello"))
      visitor.visit(program) must beEqualTo("Hello, world")
    }

    "visit a program and render a section: {{#object}}{{member}}{{/object}}" in {
      val program = Handlebars.parse("{{#greeting}}{{yo}}{{/greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = new {
          val hi = "Hi"
          val yo = "Yo"
        }
      })
      visitor.visit(program) must beEqualTo("Yo, world.")
    }

    "visit a program and render a section: {{#object}}{{../member}}{{/object}}" in {
      val program = Handlebars.parse("{{#greeting}}{{../farewell}}{{/greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "Hello"
        val farewell = "Goodbye"
      })
      visitor.visit(program) must beEqualTo("Goodbye, world.")
    }

    "visit a program and render a section, where the block context is an Option" in {
      val program = Handlebars.parse("{{#greeting}}{{toUpperCase}}{{/greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = Some("Hello")
      })
      visitor.visit(program) must beEqualTo("HELLO, world.")
    }

    "visit a program and render a section, where the block context is a Some (Primitive)" in {
      val program = Handlebars.parse("{{greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = Some("Hello")
      })
      visitor.visit(program) must beEqualTo("Hello, world.")
    }

    "visit a program and render a section, where the block context is None (Primitive)" in {
      val program = Handlebars.parse("{{greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = None
      })
      visitor.visit(program) must beEqualTo(", world.")
    }

    "visit a program and render a section, where the block context is an Iterable" in {
      val program = Handlebars.parse("{{#names}}{{toString}} & {{/names}}me")
      val visitor = HandlebarsVisitor(new {
        val names = Seq("mark","eric","mike")
      })
      visitor.visit(program) must beEqualTo("mark & eric & mike & me")
    }

    "visit a program and render a section, where the block context is an Array" in {
      val program = Handlebars.parse("{{#names}}{{toString}} & {{/names}}me")
      val visitor = HandlebarsVisitor(new {
        val names = Array("mark","eric","mike")
      })
      visitor.visit(program) must beEqualTo("mark & eric & mike & me")
    }

    "visit a program and render a section, where the block context is a Java Collection" in {
      class Person(val name: String)

      val program = Handlebars.parse("{{#people}}{{name}} & {{/people}}me")
      val names = new java.util.ArrayList[Person]
      names.add(new Person("chris"))
      names.add(new Person("yoni"))
      names.add(new Person("jim"))
      val visitor = HandlebarsVisitor(new {
        val people = names
      })
      visitor.visit(program) must beEqualTo("chris & yoni & jim & me")
    }

    "visit a program and render a section, where the the data is an guava Optional[String]" in {
      val program = Handlebars.parse("hello, {{name}}")
      val visitor = HandlebarsVisitor(new {
        val name = Optional.of("chris")
      })
      visitor.visit(program) must beEqualTo("hello, chris")
    }

    "visit a program and render a section, where the the data is an guava Optional.absent" in {
      val program = Handlebars.parse("hello, {{name}}")
      val visitor = HandlebarsVisitor(new {
        val name = Optional.absent
      })
      visitor.visit(program) must beEqualTo("hello, ")
    }

    "visit a program and render a section, where the the data is an guava Optional[Any]" in {
      class Person(val name: Optional[String])

      val program = Handlebars.parse("hello, {{person.name}}")
      val visitor = HandlebarsVisitor(new {
        val person = Optional.of(new Person(Optional.of("chris")))
      })
      visitor.visit(program) must beEqualTo("hello, chris")
    }

    "visit a program and render a section, where the data is true" in {
      val program = Handlebars.parse("{{#isTrue}}Hi!{{/isTrue}}")
      val visitor = HandlebarsVisitor(new { val isTrue = true })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and render a scetion without changing context, where the data is true" in {
      val program = Handlebars.parse("{{#value}}{{value}}{{/value}}")
      val visitor = HandlebarsVisitor(new {
          val value = true
      })
      visitor.visit(program) must beEqualTo("true")
    }

    "visit a program and omit a section, where the data is false" in {
      val program = Handlebars.parse("{{#sayHello}}Bonjour, le monde!{{/sayHello}}")
      val visitor = HandlebarsVisitor(new {
        val sayHello = false
      })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a section, where the data is undefined" in {
      val program = Handlebars.parse("{{#undefined}}Hi!{{/undefined}}")
      val visitor = HandlebarsVisitor(new {})
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a section, where the data is None" in {
      val program = Handlebars.parse("{{#doIt}}Hi!{{/doIt}}")
      val visitor = HandlebarsVisitor(new { val doIt = None })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a section, where the data is Nil" in {
      val program = Handlebars.parse("{{#doIt}}Hi!{{/doIt}}")
      val visitor = HandlebarsVisitor(new { val doIt = Nil })
      visitor.visit(program) must beEqualTo("")
    }

    "escape mustaches by default" in {
      val program = Handlebars.parse("{{greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "<strong>Hello</strong>"
      })
      visitor.visit(program) must beEqualTo("&lt;strong&gt;Hello&lt;/strong&gt;, world.")
    }

    "allow unescaped mustaches" in {
      val program = Handlebars.parse("{{{greeting}}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "<strong>Hello</strong>"
      })
      visitor.visit(program) must beEqualTo("<strong>Hello</strong>, world.")
    }

    "allow unescaped mustaches with the ampersand syntax" in {
      val program = Handlebars.parse("{{& greeting}}, world.")
      val visitor = HandlebarsVisitor(new {
        val greeting = "<strong>Hello</strong>"
      })
      visitor.visit(program) must beEqualTo("<strong>Hello</strong>, world.")
    }

    "visit a program and render an inverted section: {{^absent}}Nothing{{/absent}}" in {
      val program = Handlebars.parse("{{^absense}}Nothing to see here.{{/absense}}")
      val visitor = HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must beEqualTo("Nothing to see here.")
    }

    "visit a program and render an inverted section, where the data is false" in {
      val halQuote = "I'm sorry, Dave. I'm afraid I can't do that."
      val program = Handlebars.parse("{{^canDoThat}}" + halQuote + "{{/canDoThat}}")
      val visitor = HandlebarsVisitor(new { val canDoThat = false })
      visitor.visit(program) must beEqualTo(halQuote)
    }

    "visit a program and render an inverted section, where the data is None" in {
      val program = Handlebars.parse("{{^option}}Hi!{{/option}}")
      val visitor = HandlebarsVisitor(new { val option = None })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and render an inverted section, where the data is Nil" in {
      val program = Handlebars.parse("{{^stuff}}Hi!{{/stuff}}")
      val visitor = HandlebarsVisitor(new { val stuff = Nil })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and omit an inverted section, where the data is true" in {
      val program = Handlebars.parse("{{^isTrue}}Hi!{{/isTrue}}")
      val visitor = HandlebarsVisitor(new { val isTrue = true })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit an inverted section, where the data is defined" in {
      val program = Handlebars.parse("{{^defined}}Hi!{{/defined}}")
      val visitor = HandlebarsVisitor(new { val defined = new {}})
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and resolve a helper mustache: {{helper argument}}" in {
      val program = Handlebars.parse("{{greeting addressee}}.")
      val visitor = HandlebarsVisitor(new {
        def greeting(who: String) = {
          "Hello, " + who
        }
        val addressee = "world"
      })
      visitor.visit(program) must beEqualTo("Hello, world.")
    }

    val testHelpers: Map[String, Helper[Any]] = Map("helperName" -> ((context, options, parentContext) => {
      context.head + " " + options.hash("foo").toString + " " + options.hash("level").asInstanceOf[Long] * 2
    }));

    "visit a program and resolve a helper mustache with hash: {{helper argument foo=\"bar\"}}" in {
      val program = Handlebars.parse("""{{helperName addressee foo="bar" level=21}}.""")
      val visitor = HandlebarsVisitor(new {
        val addressee = "world"
      }, testHelpers)
      visitor.visit(program) must beEqualTo("world bar 42.")
    }

    "visit a program and resolve a helper mustache with a string literal: {{helper \"argument\"}}" in {
      val program = Handlebars.parse("{{greeting \"Dave\"}}.")
      val visitor = HandlebarsVisitor(new {
        def greeting(who: String) = {
          "Hello, " + who.toUpperCase
        }
        val context = "This is not used"
      })
      visitor.visit(program) must beEqualTo("Hello, DAVE.")
    }

    "visit a program and resolve a helper mustache with a Long literal: {{helper <argument>}}" in {
      val program = Handlebars.parse("{{double 23}}")
      val visitor = HandlebarsVisitor(new {
        def double(l: Long) = {
          l * 2
        }
      })
      visitor.visit(program) must beEqualTo("46")
    }

    "visit a program and resolve a helper mustache with a double literal: {{helper <argument>}}" in {
      val program = Handlebars.parse("{{addHandlingFee 42.30}}")
      val visitor = HandlebarsVisitor(new {
        def addHandlingFee(d: Double) = {
          d + 4.5
        }
      })
      visitor.visit(program) must beEqualTo("46.8")
    }

    "visit a program and resolve a helper mustache with doubles written different ways : {{helper 1.2 .2 3}}" in {
      val program = Handlebars.parse("{{sumFloats 1.2 .3 4}}")
      val visitor = HandlebarsVisitor(new {
        def sumFloats(d1: Double, d2: Double, d3: Double) = {
          d1 + d2 + d3
        }
      })
      visitor.visit(program) must beEqualTo("5.5")
    }

    "visit a program and resolve a helper mustache: {{helper arg1 arg2}}" in {
      val program = Handlebars.parse("{{greeting title addressee}}.")
      val visitor = HandlebarsVisitor(new {
        def greeting(title: String, who: String) = "Hello, "+title+" "+who
        val title = "Mr."
        val addressee = "Mark"
      })
      visitor.visit(program) must beEqualTo("Hello, Mr. Mark.")
    }

    "visit a program and resolve a helper expression that takes a list as an argument" in {
      val program = Handlebars.parse("{{list people}}")
      val visitor = HandlebarsVisitor(new {
        def list(names: List[String]) = names.mkString(" & ")
        val people = List("Mark","Mike","Eric")
      })
      visitor.visit(program) must beEqualTo("Mark &amp; Mike &amp; Eric")
    }

    "visit a program and evaluate a block expression" in {
      val program = Handlebars.parse("{{#head people}}Just {{name}}{{/head}}")
      val visitor = HandlebarsVisitor(new {
        def head[T](names: List[T]) = names.head
        val people = List(
          new { val name = "Mark" },
          new { val name = "Eric" },
          new { val name = "Mike" }
        )
      })
      visitor.visit(program) must beEqualTo("Just Mark")
    }

    "visit a program and find a basic block helper ({{#noop}}) in the helpers map" in {
      val program = Handlebars.parse("{{#noop}}{{name}}{{/noop}}")
      val visitor = HandlebarsVisitor(new {
        val name = "Mark"
      })
      visitor.visit(program) must beEqualTo("Mark")
    }

    "visit a program and find a block expression ({{#with}}) in the helpers map" in {
      val program = Handlebars.parse("{{#with story}}{{intro}}{{/with}}")
      val visitor = HandlebarsVisitor(new {
        val story = new {
          val intro = "Before the jump"
        }
      })
      visitor.visit(program) must beEqualTo("Before the jump")
    }

    "visit a program and find a block expression {{#if}} in the helpers map" in {
      val program = Handlebars.parse("{{#if boolean}}Hi{{/if}}")
      val visitor = HandlebarsVisitor(new {
        val boolean = true
      })
      visitor.visit(program) must beEqualTo("Hi")
    }

    "visit a program and find a block expression {{#unless}} in the helpers map" in {
      val program = Handlebars.parse("{{#unless boolean}}Hi{{/unless}}")
      val visitor = HandlebarsVisitor(new {
        val boolean = false
      })
      visitor.visit(program) must beEqualTo("Hi")
    }

    "visit a program and find a block expression {{#each}} in the helpers map" in {
      val program = Handlebars.parse("{{#each people}}!{{/each}}")
      val visitor = HandlebarsVisitor(new {
        val people = Seq("Yehuda Katz","Alan Johnson","Charles Johnson")
      })
      visitor.visit(program) must beEqualTo("!!!")
    }

    "visit a program and use the {{this}} helper to refer to the current context" in {
      val program = Handlebars.parse("{{#each people}}|{{this}}{{/each}}|")
      val visitor = HandlebarsVisitor(new {
        val people = Seq("Yehuda Katz","Alan Johnson","Charles Johnson")
      })
      visitor.visit(program) must beEqualTo("|Yehuda Katz|Alan Johnson|Charles Johnson|")
    }

    "visit a program and pass in custom helpers" in {
      val program = Handlebars.parse("{{#head people}}{{this}}{{/head}}")
      val helpers: Map[String, Handlebars.Helper[Any]] = Map(
        "head" -> ((context, option, parent) => context.head match {
          case list:Seq[_] => list.head
          case _ => context.head
        })
      )
      val visitor = HandlebarsVisitor(new {
        val people = Seq("Yehuda Katz","Alan Johnson","Charles Johnson")
      }, helpers)
      visitor.visit(program) must beEqualTo("Yehuda Katz")
    }

    "visit a program and render a block when the argument of #if is true" in {
      val program = Handlebars.parse("{{#if doIt}}Hi!{{/if}}")
      val visitor = HandlebarsVisitor(new { val doIt = true })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and omit a block when the argument of #if is false" in {
      val program = Handlebars.parse("{{#if doIt}}Hi!{{/if}}")
      val visitor = HandlebarsVisitor(new { val doIt = false })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a block when the argument of #if is undefined" in {
      val program = Handlebars.parse("{{#if doIt}}Hi!{{/if}}")
      val visitor = HandlebarsVisitor(new {})
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a block when the argument ot #if is None" in {
      val program = Handlebars.parse("{{#if doIt}}Hi!{{/if}}")
      val visitor = HandlebarsVisitor(new { val doIt = None })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and omit a block when the argument of #unless is true" in {
      val program = Handlebars.parse("{{#unless doIt}}Hi!{{/unless}}")
      val visitor = HandlebarsVisitor(new { val doIt = true })
      visitor.visit(program) must beEqualTo("")
    }

    "visit a program and render a block when the argument of #unless is false" in {
      val program = Handlebars.parse("{{#unless doIt}}Hi!{{/unless}}")
      val visitor = HandlebarsVisitor(new { val doIt = false })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and render a block when the argument of #unless is undefined" in {
      val program = Handlebars.parse("{{#unless doIt}}Hi!{{/unless}}")
      val visitor = HandlebarsVisitor(new {})
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and render a block when the argument ot #unless is None" in {
      val program = Handlebars.parse("{{#unless doIt}}Hi!{{/unless}}")
      val visitor = HandlebarsVisitor(new { val doIt = None })
      visitor.visit(program) must beEqualTo("Hi!")
    }

    "visit a program and render a block, with an else clause" in {
      val program = Handlebars.parse("{{#people}}{{name}}{{^}}{{../none}}{{/people}}")
      val visitor = HandlebarsVisitor(new {
        val none = "No people"
      })
      visitor.visit(program) must beEqualTo("No people")
    }

    "visit a program and render a block if helper, with an else clause" in {
      val program = Handlebars.parse("{{#if people}}{{name}}{{^}}{{none}}{{/if}}")
      val visitor = HandlebarsVisitor(new {
        val none = "No people"
      })
      visitor.visit(program) must beEqualTo("No people")
    }
  }

  "A Context" should {
    "get some method of the object" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      context.getMethod("greeting") must beSome
    }

    "be none when no method is there" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      context.getMethod("foobaz") must beNone
    }

    "invoke a method on the base object" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      val method = context.getMethod("greeting").get
      context.getMethod("greeting").flatMap(context.invoke(_, Nil)) must beSome("Hello, World")
    }

    "invoke a method from a name" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      context.invoke("greeting") must beSome("Hello, World")
    }

    "invoke a method or return None" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      context.invoke("foobaz") must beNone
    }

    "invoke a method with list of arguments" in {
      val hello = new {
        def greeting(who: String) = "Hello, " + who
      }
      val context = new RootContext(hello)
      context.invoke("greeting", List("World")) must beSome("Hello, World")
    }

  }

}
