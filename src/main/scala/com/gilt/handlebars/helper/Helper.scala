package com.gilt.handlebars.helper

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.parser.{DataNode, Node}
import com.gilt.handlebars.visitor.DefaultVisitor

trait Helper {
  def apply(context: Context[Any], options: HelperOptions): String
}

case class HelperOptions(args: Iterable[Any],
                         visit: (HelperContext) => String,
                         inverse: Option[(HelperContext) => String],
                         data: Map[String, Any]) extends ClassCacheableContextFactory {
  def getData(key: String): String = {
    data.get(key).map {
      case d:DataNode => createRoot(data).lookup(d).asOption.map(_.model.toString).getOrElse("")
      case nonNodeValue => nonNodeValue.toString
    }.getOrElse("")
  }

  def firstArgAsString: String = args.headOption.map(_.toString).getOrElse("")
}


object Helper extends ClassCacheableContextFactory {
  def apply(f: ((Context[Any], HelperOptions) => String)): Helper = {
    new Helper {
      def apply(context: Context[Any], options: HelperOptions): String = f(context, options)
    }
  }

  def visitFunc(context: Context[Any], node: Node, helpers: Map[String, Helper], data: Map[String, Any]) = (helperContext: HelperContext) => {
    val visitorContext = helperContext.model match {
      case c:Context[_] => c
      case anyObj => createChild(anyObj, context)
    }

    new DefaultVisitor(visitorContext, helpers, data ++ helperContext.data).visit(node)
  }

  lazy val defaultHelpers: Map[String, Helper] = Map (
    "with" -> new WithHelper,
    "if" -> new IfHelper,
    "each" -> new EachHelper,
    "log" -> new LogHelper
  )
}

case class HelperContext(model: Any, data: Map[String, Any] = Map.empty)

class StaticHelper(staticValue: Any) extends Helper {
  def apply(context: Context[Any], options: HelperOptions): String = {
    staticValue.toString
  }
}
