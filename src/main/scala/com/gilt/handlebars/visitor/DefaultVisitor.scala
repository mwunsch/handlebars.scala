package com.gilt.handlebars.visitor

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser._
import com.gilt.handlebars.parser.Content
import com.gilt.handlebars.parser.Comment
import com.gilt.handlebars.parser.Program

object DefaultVisitor extends ClassCacheableContextFactory {
  def apply[T](base: T) = {
    new DefaultVisitor[T](createRoot(base))
  }
}

class DefaultVisitor[T](context: Context[T]) extends Visitor with Loggable {
  def visit(node: Node): String = node match {
    case Content(value) => value
    case Comment(_) => ""
    case Program(statements, inverse) => statements.map(visit).mkString
    case Mustache(path, params, hash, unescaped) => {
//      println("path: %s".format(context.lookup(path)))
      escapeMustache(context.lookup(path, params).model.toString, unescaped)
    }
    case Block(mustache, program, inverse) => renderBlock(context.lookup(mustache.path), program, inverse)
    case _ => toString
  }

  protected def escapeMustache(value: String, unescaped: Boolean = true): String = {
    if (unescaped) {
      value
    } else {
      scala.xml.Utility.escape(value)
    }
  }

  protected def renderBlock(ctx: Context[Any], program: Program, inverse: Option[Program]): String = {
//    println("renderBlock: ctx: %s, program: %s, inverse: %s".format(ctx, program, inverse))
//    println("renderBlock ctx: %s".format(ctx))

    // TODO: Handle situation where context is not a Boolean, but a list or other model
    if (context.truthValue) {
      visit(program)
    } else {
      inverse.map(visit).getOrElse("")
    }
  }
}
