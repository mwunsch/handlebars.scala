package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def apply(template: String, delimiters: (String, String) = ("{{", "}}")): Handlebars = {
    new Handlebars(parse(template, delimiters))
  }

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")): Program = {
    HandlebarsGrammar(delimiters).scan(template)
  }

}

class Handlebars(program: Program) {
  def apply[T](context: T) = (new HandlebarsVisitor(context)).visit(program)
}
