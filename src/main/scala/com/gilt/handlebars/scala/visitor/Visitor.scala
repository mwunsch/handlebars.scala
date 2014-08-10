package com.gilt.handlebars.scala.visitor

import com.gilt.handlebars.scala.parser._
import com.gilt.handlebars.scala.parser.Comment
import com.gilt.handlebars.scala.parser.Content
import com.gilt.handlebars.scala.parser.Mustache
import com.gilt.handlebars.scala.parser.Block
import com.gilt.handlebars.scala.parser.Program

trait Visitor {
  def visit(program: Node): String

  def visit(program: Program): String

  def visit(content: Content): String

  def visit(comment: Comment): String

  def visit(mustache: Mustache): String

  def visit(block: Block): String

  def visit(partial: Partial): String
}
