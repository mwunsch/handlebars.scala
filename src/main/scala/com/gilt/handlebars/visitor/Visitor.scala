package com.gilt.handlebars.visitor

import com.gilt.handlebars.parser.Node

trait Visitor {
  def visit(node: Node): String
}
