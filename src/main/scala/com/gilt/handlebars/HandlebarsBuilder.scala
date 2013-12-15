package com.gilt.handlebars

import com.gilt.handlebars.parser.{HandlebarsGrammar, Program}
import com.gilt.handlebars.helper.Helper
import java.io.File
import scala.io.Source

trait HandlebarsBuilder {
  def program: Program
  def partials: Map[String, Handlebars]
  def helpers: Map[String, Helper]

  def withPartials(items: Map[String, Handlebars]): HandlebarsBuilder
  def withHelpers(items: Map[String, Helper]): HandlebarsBuilder

  def build: Handlebars
}

case class DefaultHandlebarsBuilder(program: Program,
                                    partials: Map[String, Handlebars] = Map.empty[String, Handlebars],
                                    helpers: Map[String, Helper] = Map.empty[String, Helper]) extends  HandlebarsBuilder{

  def withPartials(items: Map[String, Handlebars]) = copy(partials = partials ++ items)

  def withHelpers(items: Map[String, Helper]) = copy(helpers = helpers ++ items)

  def build: Handlebars = new HandlebarsImpl(program, partials, helpers)
}

object DefaultHandlebarsBuilder {
  def apply(template: String): DefaultHandlebarsBuilder = {
    val program = {
      val parseResult = HandlebarsGrammar(template)
      parseResult.getOrElse {
        sys.error("Could not parse template:\n\n%s\n%s".format(parseResult.next.source, parseResult.next))
      }
    }

    new DefaultHandlebarsBuilder(program)
  }

  def apply(file: File): DefaultHandlebarsBuilder = {
    val program = {
      if (file.exists()) {
        val parseResult = HandlebarsGrammar(Source.fromFile(file).mkString)
        parseResult.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
      } else {
        sys.error("Could not load template: %s".format(file.getAbsolutePath))
      }
    }

    new DefaultHandlebarsBuilder(program)
  }
}