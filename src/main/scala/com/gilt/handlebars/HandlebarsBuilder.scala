package com.gilt.handlebars

import com.gilt.handlebars.parser.{ProgramHelper, HandlebarsGrammar, Program}
import com.gilt.handlebars.helper.Helper
import java.io.File
import scala.io.Source
import com.gilt.handlebars.partial.PartialHelper
import com.gilt.handlebars.logging.Loggable

trait HandlebarsBuilder {
  def program: Program
  def partials: Map[String, Handlebars]
  def helpers: Map[String, Helper]

  def withPartials(items: Map[String, Handlebars]): HandlebarsBuilder
  def withHelpers(items: Map[String, Helper]): HandlebarsBuilder

  def build: Handlebars
}

case class DefaultHandlebarsBuilder(program: Program,
                                    partials: Map[String, Handlebars] = Map.empty,
                                    helpers: Map[String, Helper] = Map.empty) extends  HandlebarsBuilder{

  def withPartials(items: Map[String, Handlebars]) = copy(partials = partials ++ items)

  def withHelpers(items: Map[String, Helper]) = copy(helpers = helpers ++ items)

  def build: Handlebars = new HandlebarsImpl(program, partials, helpers ++ Helper.defaultHelpers)
}

object DefaultHandlebarsBuilder extends PartialHelper with ProgramHelper with Loggable {
  def apply(template: String): DefaultHandlebarsBuilder = {
    new DefaultHandlebarsBuilder(programFromString(template))
  }

  def apply(file: File): DefaultHandlebarsBuilder = {
    val partials = getTemplates(file)
    new DefaultHandlebarsBuilder(programFromFile(file), partials)
  }
}