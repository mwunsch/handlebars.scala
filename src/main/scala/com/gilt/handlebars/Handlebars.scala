package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def apply(template: String, delimiters: (String, String) = ("{{", "}}")): Handlebars = {
    new Handlebars(parse(template, delimiters))
  }

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")): Program = {
    HandlebarsGrammar(delimiters).scan(template)
  }

  type Helper[T] = (Seq[Any], HandlebarsVisitor[T], Option[T]) => Any
}

class Handlebars(program: Program) {
  import Handlebars.Helper

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = HandlebarsVisitor(context, helpers).visit(program)
}
