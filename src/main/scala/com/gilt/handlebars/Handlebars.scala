package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")) = {
    val grammar = new HandlebarsGrammar(delimiters)
    grammar.parseAll(grammar.root, template)
  }

}
