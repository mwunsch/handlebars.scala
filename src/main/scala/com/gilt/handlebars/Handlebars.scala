package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")): Program = {
    HandlebarsGrammar(delimiters).scan(template)
  }

  def eval(context: Any, program: Program) = {

  }

}
