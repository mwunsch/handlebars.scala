package com.gilt.handlebars

import com.gilt.handlebars.parser._
import java.io.File
import com.gilt.handlebars.visitor.DefaultVisitor
import com.gilt.handlebars.helper.Helper
import com.gilt.handlebars.partial.PartialHelper


trait Handlebars {
  def program: Program

  def apply[T](context: T,
               data: Map[String, Any] = Map.empty,
               partials: Map[String, Handlebars] = Map.empty,
               helpers: Map[String, Helper] = Map.empty): String

  def partials: Map[String, Handlebars]

  def helpers: Map[String, Helper]
}

class HandlebarsImpl(override val program: Program,
                     override val partials: Map[String, Handlebars],
                     override val helpers: Map[String, Helper]) extends Handlebars with PartialHelper {

  // TODO: check program for partials that are not in the partials map. See if they exist as strings in data
  override def apply[T](context: T,
                        data: Map[String, Any] = Map.empty,
                        providedPartials: Map[String, Handlebars] = Map.empty,
                        providedHelpers: Map[String, Helper] = Map.empty): String = {
    DefaultVisitor(context, normalizePartialNames(partials ++ providedPartials), helpers ++ providedHelpers, data).visit(program)
  }
}

object Handlebars {
  def apply(template: String): Handlebars = createBuilder(template).build
  def apply(file: File): Handlebars = createBuilder(file).build

  def createBuilder(template: String): DefaultHandlebarsBuilder = DefaultHandlebarsBuilder(template)
  def createBuilder(file: File): DefaultHandlebarsBuilder = DefaultHandlebarsBuilder(file)
}
