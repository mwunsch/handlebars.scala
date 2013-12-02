package com.gilt.handlebars

import com.gilt.handlebars.parser._
import java.io.File
import scala.io.Source
import com.gilt.handlebars.visitor.DefaultVisitor

/**
 * User: chicks
 * Date: 6/29/13
 */
trait Handlebars {
  def program: Program

  def apply[T](context: T): String
}

object Handlebars {
  def apply(template: String): Handlebars = {
    new Handlebars {
      def apply[T](context: T): String = DefaultVisitor(context).visit(program)

      def program: Program = {
        val parseResult = HandlebarsGrammar(template)
        parseResult.getOrElse {
          sys.error("Could not parse template:\n\n%s\n%s".format(parseResult.next.source, parseResult.next))
        }
      }
    }
  }

  def apply(file: File): Handlebars = {
    new Handlebars {
      def apply[T](context: T): String = "" // Call to HandlebarsVisitor

      def program: Program = {
        if (file.exists()) {
          val parseResult = HandlebarsGrammar(Source.fromFile(file).mkString)
          parseResult.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
        } else {
          sys.error("Could not load template: %s".format(file.getAbsolutePath))
        }
      }
    }
  }
}
