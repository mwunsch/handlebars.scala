package com.gilt.handlebars.context

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Date
import com.gilt.handlebars.parser.{Mustache, Identifier, Program, HandlebarsGrammar}

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

class ContextSpec extends FunSpec with ShouldMatchers with ClassCacheableContextFactory {
  import ContextSpec._

  val sale = Sale("Sneakers!", new Date(), List(Stores.WOMEN, Stores.MEN))
  val product = Product("Chuck Taylor", "Converse", Some(ProductContent("Description", "Title")))
  val productNoContent = Product("Vaider", "Supra", None)
  val person = Person("Zach", "Smith")

  describe("Lookup") {
    it("looks up a path: 'name'") {
      val rootCtx = createRoot(product)
      rootCtx.lookup(getIdentifier("{{name}}")).model should equal("Chuck Taylor")
    }

    it("looks up a path: '../name'") {
      val ctx = createChild(product, createRoot(sale))
      ctx.lookup(getIdentifier("{{../name}}")).model should equal("Sneakers!")
    }

    it("looks up a path: '../../name'") {
      val ctx = createChild(person, createChild(product, createRoot(sale)))
      ctx.lookup(getIdentifier("{{../../name}}")).model should equal("Sneakers!")
    }

    it("looks up a path: '../content.title'") {
      val ctx = createChild(sale, createRoot(product))
      ctx.lookup(getIdentifier("{{../content.title}}")).model should equal("Title")
    }

    it("looks up a path: 'this'") {
      val ctx = createRoot(person)
      ctx.lookup(getIdentifier("{{this}}")).model should equal(person)
    }

    it("looks up a path with empty Option: '../content.description'") {
      val ctx = createChild(person, createRoot(productNoContent))
      ctx.lookup(getIdentifier("{{../content.description}}")).isUndefined should equal(true)
    }

    it("looks up a path that is undefined: 'undefined'") {
      val ctx = createRoot(product)
      ctx.lookup(getIdentifier("{{undefined}}")).isUndefined should equal(true)
    }

    it("looks up a path that is undefined: '../../undefined'") {
      val ctx = createChild(person, createChild(product, createRoot(sale)))
      ctx.lookup(getIdentifier("{{../../undefined}}")).isUndefined should equal(true)
    }

    it("looks up a path that is undefined: '../content.undefined'") {
      val ctx = createChild(sale, createRoot(product))
      ctx.lookup(getIdentifier("{{../content.undefined}}")).isUndefined should equal(true)
    }

    it("looks up a path with a list: '../name'") {
      val ctx = createChild(Stores.WOMEN, createChild(sale.stores, createRoot(sale)))
      ctx.lookup(getIdentifier("{{../name}}")).model should equal("Sneakers!")
    }

    it("looks up a path with list root parent: '../name'") {
      val ctx = createChild(Stores.WOMEN, createRoot(sale.stores))
      ctx.lookup(getIdentifier("{{../name}}")).isUndefined should equal(true)
    }

    it("looks up a path with nonexistent ancestors, but stays on root node: '../../../../../../../name'") {
      val ctx = createChild(sale, createRoot(product))
      ctx.lookup(getIdentifier("{{../../../../../../../name}}")).model.toString should equal("Chuck Taylor")
    }

    it("looks up a path with nonexistent children: 'content.description.undefined.undefined'") {
      val ctx = createRoot(product)
      ctx.lookup(getIdentifier("{{content.description.undefined.undefined}}")).isUndefined should equal(true)
    }

    it("looks up a path in a Map: 'city'") {
      val ctx = createRoot(Map("city" -> "Brussels"))
      ctx.lookup(getIdentifier("{{city}}")).model should equal("Brussels")
    }

    it("looks up a path in a Map: '../city'") {
      val ctx = createChild(Map("country" -> "Belgium"), createRoot(Map("city" -> "Brussels")))
      ctx.lookup(getIdentifier("{{../city}}")).model should equal("Brussels")
    }

    it("looks up a path in a Map: 'country.city'") {
      val ctx = createRoot(Map("country" -> Map("city" -> "Brussels")))
      ctx.lookup(getIdentifier("{{country.city}}")).model should equal("Brussels")
    }

    it("looks up a path in a Map as Iterable: '../name'") {
      val ctx = createChild(person, createChild(Map(None -> "as iterable - skip!"), createRoot(sale)))
      ctx.lookup(getIdentifier("{{../name}}")).model should equal("Sneakers!")
    }

    it("looks up a path using a literal: '[@alan].expression'") {
      val ctx = createRoot(Map("@alan" -> Map("expression" -> "beautiful")))
      ctx.lookup(getIdentifier("{{[@alan].expression}}")).model should equal("beautiful")
    }

    it("looks up a path using a literal: '[foo bar].expression'") {
      val ctx = createRoot(Map("foo bar" -> Map("expression" -> "beautiful")))
      ctx.lookup(getIdentifier("{{[foo bar].expression}}")).model should equal("beautiful")
    }

    it("looks up a path incorrectly using a literal: '[alan].expression'") {
      val ctx = createRoot(person)
      ctx.lookup(getIdentifier("{{[@alan].expression}}")).isUndefined should equal(true)
    }
  }
}
