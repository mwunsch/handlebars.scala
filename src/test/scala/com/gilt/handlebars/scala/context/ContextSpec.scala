package com.gilt.handlebars.scala.context

import org.scalatest.{ FunSpec, Matchers }
import java.util.Date
import com.gilt.handlebars.scala.parser.{Mustache, Identifier, Program, HandlebarsGrammar}
import com.gilt.handlebars.scala.binding.dynamic._

/**
 * User: chicks
 * Date: 5/30/13
 * Time: 10:27 AM
 */
object Stores {
  val WOMEN = Store("women", "Women", 1l)
  val MEN = Store("men", "Men", 2l)
  val CHILDREN = Store("children", "Baby & Kids", 3l)
  val HOME = Store("home", "Home", 4l)
}

object ContextSpec {
  def getIdentifier(str: String): Identifier = {
    getIdentifier(HandlebarsGrammar.apply(str).get)
  }
  def getIdentifier(program: Program): Identifier = {
    program.statements.headOption.map(_.asInstanceOf[Mustache].path.asInstanceOf[Identifier]).getOrElse(sys.error("Expected: Program similar to Program(List(Mustache(Identifier(List(...)),List(),HashNode(Map()),false)),None)"))
  }
}

case class Store(key: String, displayName: String, storeId: Long)
case class Sale(name: String, startDate: Date, stores: Iterable[Store])
case class Product(name: String, brand: String, content: Option[ProductContent] = None)
case class ProductContent(description: String, title: String)
case class Person(firstName: String, lastName: String)

class ContextSpec extends FunSpec with Matchers {
  import ContextSpec._

  val sale = Sale("Sneakers!", new Date(), List(Stores.WOMEN, Stores.MEN))
  val product = Product("Chuck Taylor", "Converse", Some(ProductContent("Description", "Title")))
  val productNoContent = Product("Vaider", "Supra", None)
  val person = Person("Zach", "Smith")

  implicit class ContextForTesting[T](c:Context[T]) {
    def modelValue = c.binding.get
  }
  describe("Lookup") {
    def makeChildContext(parent: Context[Any], childModelValue: Any) = {
      parent.childContext(DynamicBinding(childModelValue))
    }

    // Creates a context with the provided values linked up in a parent/child chain; returns childest context.
    def makeContext(root: Any, children: Any* ) =
      children.foldLeft(Context(root))(makeChildContext)

    it("looks up a path: 'name'") {
      val rootCtx = Context(product)
      rootCtx.lookup(getIdentifier("{{name}}")).modelValue should equal("Chuck Taylor")
    }

    it("looks up a path: '../name'") {
      val ctx = makeContext(sale, product)
      ctx.lookup(getIdentifier("{{../name}}")).modelValue should equal("Sneakers!")
    }

    it("looks up a path: '../../name'") {
      val ctx = makeContext(sale, product, person)
      ctx.lookup(getIdentifier("{{../../name}}")).modelValue should equal("Sneakers!")
    }

    it("looks up a path: '../content.title'") {
      val ctx = makeContext(product, sale)
      ctx.lookup(getIdentifier("{{../content.title}}")).modelValue should equal("Title")
    }

    it("looks up a path: 'this'") {
      val ctx = makeContext(person)
      ctx.lookup(getIdentifier("{{this}}")).modelValue should equal(person)
    }

    it("looks up a path with empty Option: '../content.description'") {
      val ctx = makeContext(productNoContent, person)
      ctx.lookup(getIdentifier("{{../content.description}}")).isVoid should equal(true)
    }

    it("looks up a path that is undefined: 'undefined'") {
      val ctx = makeContext(product)
      ctx.lookup(getIdentifier("{{undefined}}")).isVoid should equal(true)
    }

    it("looks up a path that is undefined: '../../undefined'") {
      val ctx = makeContext(sale, product, person)
      ctx.lookup(getIdentifier("{{../../undefined}}")).isVoid should equal(true)
    }

    it("looks up a path that is undefined: '../content.undefined'") {
      val ctx = makeContext(product, sale)
      ctx.lookup(getIdentifier("{{../content.undefined}}")).isVoid should equal(true)
    }

    it("looks up a path with a list: '../name'") {
      val ctx = makeContext(sale, sale.stores, Stores.WOMEN)
      ctx.lookup(getIdentifier("{{../name}}")).modelValue should equal("Sneakers!")
    }

    it("looks up a path with list root parent: '../name'") {
      val ctx = makeContext(sale.stores, Stores.WOMEN)
      ctx.lookup(getIdentifier("{{../name}}")).isVoid should equal(true)
    }

    it("looks up a path with nonexistent ancestors, but stays on root node: '../../../../../../../name'") {
      val ctx = makeContext(product, sale)
      ctx.lookup(getIdentifier("{{../../../../../../../name}}")).modelValue.toString should equal("Chuck Taylor")
    }

    it("looks up a path with nonexistent children: 'content.description.undefined.undefined'") {
      val ctx = makeContext(product)
      ctx.lookup(getIdentifier("{{content.description.undefined.undefined}}")).isVoid should equal(true)
    }

    it("looks up a path in a Map: 'city'") {
      val ctx = makeContext(Map("city" -> "Brussels"))
      ctx.lookup(getIdentifier("{{city}}")).modelValue should equal("Brussels")
    }

    it("looks up a path in a Map: '../city'") {
      val ctx = makeContext(Map("city" -> "Brussels"), person)
      ctx.lookup(getIdentifier("{{../city}}")).modelValue should equal("Brussels")
    }

    it("looks up a path in a Map: 'country.city'") {
      val ctx = makeContext(Map("country" -> Map("city" -> "Brussels")))
      ctx.lookup(getIdentifier("{{country.city}}")).modelValue should equal("Brussels")
    }

    it("looks up a path in a Map as Iterable: '../name'") {
      val ctx = makeContext(sale, Map(None -> "as iterable - skip!"), person)
      ctx.lookup(getIdentifier("{{../name}}")).modelValue should equal("Sneakers!")
    }

    it("looks up a path using a literal: '[@alan].expression'") {
      val ctx = makeContext(Map("@alan" -> Map("expression" -> "beautiful")))
      ctx.lookup(getIdentifier("{{[@alan].expression}}")).modelValue should equal("beautiful")
    }

    it("looks up a path using a literal: '[foo bar].expression'") {
      val ctx = makeContext(Map("foo bar" -> Map("expression" -> "beautiful")))
      ctx.lookup(getIdentifier("{{[foo bar].expression}}")).modelValue should equal("beautiful")
    }

    it("looks up a path incorrectly using a literal: '[alan].expression'") {
      val ctx = makeContext(person)
      ctx.lookup(getIdentifier("{{[@alan].expression}}")).isVoid should equal(true)
    }
  }
}
