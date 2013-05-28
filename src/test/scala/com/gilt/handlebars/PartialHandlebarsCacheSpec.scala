package com.gilt.handlebars

import org.specs2.mutable.Specification
import java.io.File

/**
 * User: chicks
 * Date: 5/28/13
 * Time: 12:59 PM
 */
class PartialHandlebarsCacheSpec extends Specification {
  "PartialHandlebarsCacheSpec" should {
    "not infinite loop with recursive partials" in {
      val template = Handlebars.fromFile(new File("src/test/resources/recursiveMain.handlebars"))
      val testData = new {
        val number = 1
        val data = new {
          val number = 2
          val data = new {
            val number = 3
          }
        }
      }

      template(testData).trim must beEqualTo("""Recursing on self: 1
                                           |Recursing on self: 2
                                           |Recursing on self: 3""".stripMargin)
    }

    "not infinite loop with circular dependencies" in {
      case class Person(firstName: String, age: Int)
      val template = Handlebars.fromFile(new File("src/test/resources/person.handlebars"))
      val testData = new {
        val person = Person("Chris", 12)
        val intermediate = new {
          val somethingElse = new {
            val person = Person("Rich", 45)
          }
        }
      }

      template(testData).trim must beEqualTo("""Chris -> 12
                                          |
                                          |Rich -> 45""".stripMargin)
    }
  }

}
