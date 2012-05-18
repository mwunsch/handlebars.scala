package com.gilt.handlebars

import org.specs2.mutable._
import com.gilt.handlebars._

class HandlebarsVisitorSpec extends Specification {
  "A Handlebars Visitor" should {
    "visit a program with only content" in {
      val program = Handlebars.parse("bar")
      val visitor = new HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must beEqualTo("bar")
    }

    "visit a program with a simple mustache" in {
      val program = Handlebars.parse("{{head}} == foo")
      val visitor = new HandlebarsVisitor(Seq("foo","bar","baz"))
      visitor.visit(program) must beEqualTo("foo == foo")
    }

    "silently fail with an empty string when a mustache can not be resolved" in {
      val program = Handlebars.parse("{{foo}} is empty")
      val visitor = new HandlebarsVisitor("A Context[String]")
      visitor.visit(program) must beEqualTo("is empty").ignoreSpace
    }

    "visit a program with the mustache containg a path: {{foo/bar}}" in {
      val program = Handlebars.parse("{{greeting/toUpperCase}}, world.")
      val visitor = new HandlebarsVisitor(new {
        val greeting = "Hello"
      })
      visitor.visit(program) must beEqualTo("HELLO, world.")
    }

    "visit a program with the mustache containg a path: {{foo/../bar}}" in {
      val program = Handlebars.parse("{{greeting/../farewell}}, world.")
      val visitor = new HandlebarsVisitor(new {
        val greeting = "Hello"
        val farewell = "Goodbye"
      })
      visitor.visit(program) must beEqualTo("Goodbye, world.")
    }

    "visit a program with the mustache containg a path: {{foo/bar/../baz}}" in {
      val program = Handlebars.parse("{{greeting/hi/../yo}}, world.")
      val visitor = new HandlebarsVisitor(new {
        val greeting = new {
          val hi = "Hi"
          val yo = "Yo"
          override def toString = "Hello"
        }
      })
      visitor.visit(program) must beEqualTo("Yo, world.")
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
      context.getMethod("greeting").flatMap(context.invoke) must beSome("Hello, World")
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
  }

}
