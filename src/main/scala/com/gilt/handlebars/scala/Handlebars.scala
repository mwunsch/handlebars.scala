package com.gilt.handlebars.scala

import java.io.File

import com.gilt.handlebars.scala.binding.{Binding, BindingFactory}
import com.gilt.handlebars.scala.context.Context
import com.gilt.handlebars.scala.helper.Helper
import com.gilt.handlebars.scala.parser._
import com.gilt.handlebars.scala.partial.PartialHelper
import com.gilt.handlebars.scala.visitor.DefaultVisitor

trait Handlebars[T] {
  def program: Program

  def partials: Map[String, Handlebars[T]]

  def helpers: Map[String, Helper[T]]

  def apply(
    context: Binding[T],
    data: Map[String, Binding[T]] = Map.empty[String, Binding[T]],
    partials: Map[String, Handlebars[T]] = Map.empty[String, Handlebars[T]],
    helpers: Map[String, Helper[T]] = Map.empty[String, Helper[T]])(implicit c: BindingFactory[T]): String
}

class HandlebarsImpl[T](
  override val program: Program,
  override val partials: Map[String, Handlebars[T]],
  override val helpers: Map[String, Helper[T]])
    extends Handlebars[T] {

  // TODO: check program for partials that are not in the partials map. See if they exist as strings in data
  override def apply(
    binding: Binding[T],
    data: Map[String, Binding[T]] = Map.empty[String, Binding[T]],
    providedPartials: Map[String, Handlebars[T]] = Map.empty[String, Handlebars[T]],
    providedHelpers: Map[String, Helper[T]] = Map.empty[String, Helper[T]])(implicit c: BindingFactory[T]): String = {

    DefaultVisitor(Context(binding), PartialHelper.normalizePartialNames(partials ++ providedPartials), helpers ++ providedHelpers, data, Map.empty[String, Binding[T]]).visit(program)
  }
}

object Handlebars {
  def apply[T](template: String)(implicit c: BindingFactory[T]): Handlebars[T] = createBuilder(template).build
  def apply[T](file: File)(implicit c: BindingFactory[T]): Handlebars[T] = createBuilder(file).build

  def createBuilder[T](template: String)(implicit c: BindingFactory[T]): DefaultHandlebarsBuilder[T] = DefaultHandlebarsBuilder(template)
  def createBuilder[T](file: File)(implicit c: BindingFactory[T]): DefaultHandlebarsBuilder[T] = DefaultHandlebarsBuilder(file)
}
