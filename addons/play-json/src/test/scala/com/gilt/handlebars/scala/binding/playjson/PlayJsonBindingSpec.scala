package com.gilt.handlebars.scala.binding.playjson

import org.scalatest.{FunSpec,Matchers}
import play.api.libs.json._

class PlayJsonBindingSpec extends FunSpec with Matchers {
  def b(v: JsValue): PlayJsonBinding = new PlayJsonBinding(v)
  def b(v: String): PlayJsonBinding = b(Json.parse(v))

  describe("isTruthy") {
    it("matches JavaScript truthy behavior") {
      b(JsString("")).isTruthy should be(false)
      b(JsString("something")).isTruthy should be(true)

      b(JsBoolean(false)).isTruthy should be(false)
      b(JsBoolean(true)).isTruthy should be(true)

      b(JsNumber(0)).isTruthy should be(false)
      b(JsNumber(1)).isTruthy should be(true)

      b(JsObject(Seq())).isTruthy should be(true)
      b(JsArray(Seq())).isTruthy should be(true)
    }
  }

  describe("#traverse") {
    val objBinding = b("""{"name": "Mike"}""")
    it("traverses an object") {
      objBinding.traverse("name").render should be("Mike")
    }

    it("goes into the void for undefined keys") {
      objBinding.traverse("boogie").isDefined should be(false)
    }

    it("goes into the void when trying to traverse a non-object") {
      b(JsNumber(0)).traverse("boogie").isDefined should be(false)
    }
  }

  describe("arrays") {
    val arrayBinding = b("""[1,2,3]""")
    it("#isCollection returns true for arrays, but not objects") {
      arrayBinding.isCollection should be(true)
    }
    it("#asCollection turns a collection into a Seq of bindings") {
      arrayBinding.asCollection.map(_.render) should be(Seq("1", "2", "3"))
    }
  }
  describe("asDictionaryCollection") {
    it("turns a dictionary into a Seq of bindings") {
      b("""{"a": 1, "b": 2}""").asDictionaryCollection.toSeq.map { case (k,v) => (k, v.get) } should be(Seq("a" -> JsNumber(1), "b" -> JsNumber(2)))
    }
  }
  describe("getOrElse") {
    it("returns the contents") {
      val test = ((b(JsNull).getOrElse { JsString("dance") }))
      test.toString should equal ("null")
    }
  }
}
