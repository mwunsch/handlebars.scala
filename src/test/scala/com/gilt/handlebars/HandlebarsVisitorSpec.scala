package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._

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

    "visit a program and render a section, where the block context is an Iterable" in {
      val program = Handlebars.parse("{{#names}}{{toString}} & {{/names}}me")
      val visitor = HandlebarsVisitor(new {
        val names = Seq("mark","eric","mike")
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

    "visit a program and resolve a helper mustache: {{helper argument}}" in {
      val program = Handlebars.parse("{{greeting addressee}}.")
      val visitor = HandlebarsVisitor(new {
        def greeting(who: String) = "Hello, "+who
        val addressee = "world"
      })
      visitor.visit(program) must beEqualTo("Hello, world.")
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
