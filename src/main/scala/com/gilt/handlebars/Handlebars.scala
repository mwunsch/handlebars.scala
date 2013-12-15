package com.gilt.handlebars

import com.gilt.handlebars.parser._
import java.io.File
import com.gilt.handlebars.visitor.DefaultVisitor
import com.gilt.handlebars.helper.Helper


trait Handlebars {
  def program: Program

  def apply[T](context: T): String

  def partials: Map[String, Handlebars]

  def helpers: Map[String, Helper]
}

class HandlebarsImpl(override val program: Program,
                     override val partials: Map[String, Handlebars],
                     override val helpers: Map[String, Helper]) extends Handlebars {
  override def apply[T](context: T): String = DefaultVisitor(context, helpers).visit(program)
}

object Handlebars {
  def apply(template: String): Handlebars = createBuilder(template).build
  def apply(file: File): Handlebars = createBuilder(file).build

  def createBuilder(template: String): DefaultHandlebarsBuilder = DefaultHandlebarsBuilder(template)
  def createBuilder(file: File): DefaultHandlebarsBuilder = DefaultHandlebarsBuilder(file)
}
