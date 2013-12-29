package com.gilt.handlebars.helper

import com.gilt.handlebars.context.{ClassCacheableContextFactory, ClassCacheableContext, Context}
import com.gilt.handlebars.parser.Node
import com.gilt.handlebars.visitor.DefaultVisitor

/**
 * User: chicks
 * Date: 12/14/13
 */
trait Helper {
  def apply(context: Context[Any], args: Iterable[Any], visit: (Any) => String, inverse: Option[(Any) => String]): String
}

object Helper extends ClassCacheableContextFactory {
  def apply(f: ((Context[Any], Iterable[Any], (Any) => String, Option[(Any) => String]) => String)): Helper = {
    new Helper {
      def apply(context: Context[Any], args: Iterable[Any], visit: (Any) => String, inverse: Option[(Any) => String]): String = f(context, args, visit, inverse)
    }
  }

  def visitFunc(context: Context[Any], node: Node, helpers: Map[String, Helper]) = (model: Any) => {
    val ctx = model match {
      case c:Context[_] => c
      case anyObj => createChild(anyObj, context)
    }
//    println("Visit func called - ctx: %s, node: %s".format(ctx, node))
    new DefaultVisitor(ctx, helpers).visit(node)
  }
}

class StaticHelper(staticValue: Any) extends Helper {
  def apply(context: Context[Any], args: Iterable[Any], visit: (Any) => String, inverse: Option[(Any) => String]): String = {
    staticValue.toString
  }
}
