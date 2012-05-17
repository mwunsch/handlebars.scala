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
      context.invoke("greeting") must beEqualTo("Hello, World")
    }

    "invoke a method or return an empty string" in {
      val hello = new { val greeting = "Hello, World" }
      val context = new RootContext(hello)
      context.invoke("foobaz") must be empty
    }
  }

}
