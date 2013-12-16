package com.gilt.handlebars.visitor

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser._
import com.gilt.handlebars.parser.Content
import com.gilt.handlebars.parser.Comment
import com.gilt.handlebars.parser.Program
import com.gilt.handlebars.helper.{StaticHelper, Helper}

object DefaultVisitor extends ClassCacheableContextFactory {
  def apply[T](base: T, helpers: Map[String, Helper]) = {
    new DefaultVisitor[T](createRoot(base), helpers)
  }
}

class DefaultVisitor[T](context: Context[T], helpers: Map[String, Helper]) extends Visitor with Loggable with ClassCacheableContextFactory {
  def visit(node: Node): String = node match {
    case Content(value) => value
    case Comment(_) => ""
    case Program(statements, inverse) => statements.map(visit).mkString
    case mustache:Mustache/*(path, params, hash, unescaped)*/ => {

//      println("path %s, context lookup: %s".format(mustache.path, context.lookup(mustache.path, mustache.params)))
      val value = context.lookup(mustache.path, mustache.params).asOption.map {
        _.model.toString
      }.orElse {
//        println("params: %s".format(params))
        helpers.get(mustache.path.string).map(callHelper(_, context, mustache, mustache.params))
      }.getOrElse {
        warn("Could not find path or helper: %s".format(mustache.path))
        ""
      }

      println("mustache: %s -> %s".format(mustache.path.string, value))

      escapeMustache(value, mustache.unescaped)
    }
    case Block(mustache, program, inverse) => {
      println("block: %s -> %s".format(mustache.path.string, context.lookup(mustache.path)))
      val lookedUpCtx = context.lookup(mustache.path)
      lookedUpCtx.asOption.map {
        ctx =>
          renderBlock(ctx, program, inverse)
      }.orElse {
        println("--> program: %s".format(program))
        helpers.get(mustache.path.string).map(callHelper(_, context, program, mustache.params))
      }.getOrElse {
//        warn("Could not find path or helper for block: %s".format(mustache.path.string))
        renderBlock(lookedUpCtx, program, inverse)
      }
    }
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
//    println("renderBlock: context - %s".format(context))

    if (ctx.truthValue) {
      ctx.model match {
        case l:Iterable[_] => l.zipWithIndex.map {
          // TODO: pass index as 'data' when that is implemented, NOT a helper
          case (item, idx) => new DefaultVisitor(createChild(item, ctx), helpers + ("index" -> new StaticHelper(idx))).visit(program)
        }.mkString
        case _ => visit(program)
      }
    } else {
      inverse.map(visit).getOrElse("")
    }
  }

  protected def callHelper(helper: Helper, context: Context[Any], program: Node, params: List[ValueNode]): String = {
    val args = params.map {
      case i:Identifier => {
//        println("helper context: %s, path: %s, lookup -> %s".format(context.model, i, context.lookup(i).model))
        context.lookup(i).asOption.map(_.model).getOrElse {
          warn("Path not found for helper: %s".format(i.string))
          ""
        }
      }
      case _ => toString
    }

    helper.apply(context, args, Helper.visitFunc(context, program, helpers))
  }
}
