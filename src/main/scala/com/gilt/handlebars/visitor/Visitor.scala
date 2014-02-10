package com.gilt.handlebars.visitor

import com.gilt.handlebars.parser._
import com.gilt.handlebars.parser.Comment
import com.gilt.handlebars.parser.Content
import com.gilt.handlebars.parser.Mustache
import com.gilt.handlebars.parser.Block
import com.gilt.handlebars.parser.Program

trait Visitor {
  def visit(program: Node): String

  def visit(program: Program): String

  def visit(content: Content): String

  def visit(comment: Comment): String

  def visit(mustache: Mustache): String

  def visit(block: Block): String

  def visit(partial: Partial): String
}
