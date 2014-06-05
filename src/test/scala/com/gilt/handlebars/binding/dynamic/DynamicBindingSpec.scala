package com.gilt.handlebars.binding.dynamic

import org.scalatest.{ FunSpec, Matchers }
import com.gilt.handlebars.binding.VoidBinding
import com.gilt.handlebars.binding.FullBinding

class DynamicBindingSpec extends FunSpec with Matchers {
  def d(v: Any) : String = {
    if (v == null) return "null"
    v match {
      case Some(a) => s"Some(${d(a)})"
      case Unit => "Unit"
      case a: String => "\"%s\"" format a
      case _ => v.toString
    }
  }

  describe("isTruthy") {
    for {
      (value, truthy) <- List(
        (List()      -> false),
        (null         , false),
        (None        -> false),
        (""          -> false),
        (Unit        -> false),
        (0           -> true),
        ("non-empty" -> true))
    } it(s"with ${d(value)} returns $truthy") {
      DynamicBinding(value).isTruthy should equal(truthy)
    }
  }

  describe("toOption") {
    for {
      (value, result) <- List[(Any, Option[Any])](
        (null    , None),
        (None   -> None),
        (Unit   -> None),
        (List() -> Some(List())),
        (""     -> Some("")),
        (0      -> Some(0)))
    } it(s"with ${d(value)} returns ${d(result)}") {
      DynamicBinding(value).toOption should equal(result)
    }
  }

  describe("render") {
    for {
      (value, expectation) <- List(
        (List()      -> ""),
        (null         , ""),
        (None        -> ""),
        (""          -> ""),
        (Unit        -> ""),
        (0           -> "0"),
        (false       -> ""),
        ("non-empty" -> "non-empty"))
    } it(s"with ${d(value)} returns ${d(expectation)}") {
      DynamicBinding(value).render should equal(expectation)
    }
  }


  describe("traverse") {
    it("traverses methods in classes") {
      case class Person(name: String)
      DynamicBinding(Person("Bob")).traverse("name") should equal (DynamicBinding("Bob"))
    }

    it("traverses string members in maps") {
      DynamicBinding(Map("name" -> "Bob")).traverse("name") should equal (DynamicBinding("Bob"))
    }

    it("returns a VoidBinding when traversing to the unknown") {
      DynamicBinding(Map("name" -> "Bob")).traverse("age") should equal (VoidBinding[Any])
    }
  }
  describe("asCollection") {
    it("returns a list of bindings for iterables") {
      DynamicBinding(List(1,2).toIterable).asCollection should equal (List(DynamicBinding(1), DynamicBinding(2)))
    }
    it("returns an empty list for non-collections") {
      DynamicBinding("Hi").asCollection should equal (List())
    }
    it("returns an empty list for maps") {
      DynamicBinding(Map("a" -> 1)).asCollection should equal (List())
    }
  }

  describe("asOption") {
    for {
      (value, expectation) <- List(
        (List()      -> Some(DynamicBinding(List()))),
        (null         , None),
        (None        -> None),
        (""          -> Some(DynamicBinding(""))),
        (Unit        -> None),
        (0           -> Some(DynamicBinding(0))),
        (false       -> Some(DynamicBinding(false))))
    } it(s"with ${d(value)} returns ${d(expectation)}") {
      DynamicBinding(value).asOption should equal(expectation)
    }
  }

  describe("asDictionaryCollection") {
    it("returns empty for non-maps") {
      DynamicBinding("Hi").asDictionaryCollection should equal (List())
    }
    it("returns empty for iterables") {
      DynamicBinding(List(1,2).toIterable).asDictionaryCollection should equal (List())
    }
    it("returns a tuple of (key, Binding(value)) for each map item") {
      DynamicBinding(Map("a" -> 1, "b" -> 2)).asDictionaryCollection should equal (List("a" -> DynamicBinding(1), "b" -> DynamicBinding(2)))
    }
  }

  describe("isCollection") {
    it("returns true if bound to a iterable") {
      DynamicBinding(List(1,2).toIterable).isCollection should equal (true)
    }
    it("returns false if bound to a map") {
      DynamicBinding(Map("a" -> 1)).isCollection should equal (false)
    }
    it("returns false if bound to a non-collection") {
      DynamicBinding("hello, world").isCollection should equal (false)
    }
  }
  describe("isDictionary") {
    it("returns true if bound to a map") {
      DynamicBinding(Map("a" -> 1)).isDictionary should equal (true)
    }
    it("returns false if bound to a iterable") {
      DynamicBinding(List(1,2).toIterable).isDictionary should equal (false)
    }
  }
  describe("get") {
    it("returns the containing value") {
      DynamicBinding("hi").get should equal("hi")
    }
  }

  describe("unapply") {
    it("yields the value when pattern matching") {
      (DynamicBinding(1) match {
        case FullBinding(v) => v
        case VoidBinding => 0
      }) should equal (1)
    }
  }
}
