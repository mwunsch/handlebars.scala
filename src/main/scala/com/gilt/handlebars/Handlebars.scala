package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")) = {
    HandlebarsGrammar(delimiters).scan(template)
  }

}
