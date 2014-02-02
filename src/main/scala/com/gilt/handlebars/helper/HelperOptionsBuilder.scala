package com.gilt.handlebars.helper

import com.gilt.handlebars.parser._
import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.Handlebars
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser.Program
import com.gilt.handlebars.visitor.DefaultVisitor

trait HelperOptions {
  def argument(index: Int): Option[Any]
  def data(key: String): String

  def visit(model: Any): String
  def visit(model: Any, extraData: Map[String, Any]): String
  def inverse(model: Any): String
  def inverse(model: Any, extraData: Map[String, Any]): String

  def lookup(path: String): Option[Any]
}

class HelperOptionsBuilder(context: Context[Any],
                           partials: Map[String, Handlebars],
                           helpers: Map[String, Helper],
                           data: Map[String, Any],
                           program: Node,
                           params: List[ValueNode]) extends ClassCacheableContextFactory with Loggable {

  private val args = params.map {
    case i:IdentifierNode => {
      // 1. Look in the Context
      context.lookup(i).asOption.map(_.model).orElse {
        // 2. Check the global data, but treat it as a context in case the path is 'foo.bar'
        createRoot(data).lookup(i).asOption.map(_.model)
      }.getOrElse {
        // 3. Give up, path wasn't found anywhere
        warn("Path not found for helper: %s".format(i.string))
        ""
      }
    }
    case a => a.toString
  }

  private val inverseNode: Option[Node] = program match {
    case p:Program => p.inverse
    case b:Block => b.inverse
    case _ => None
  }

  def build: HelperOptions = new HelperOptionsImpl(args, data)

  private class HelperOptionsImpl(args: List[Any],
                                  dataMap: Map[String, Any]) extends HelperOptions with ClassCacheableContextFactory {

    def argument(index: Int): Option[Any] = {
      args.lift(index)
    }

    def data(key: String): String = {
      dataMap.get(key).map {
        case d:DataNode => createRoot(dataMap).lookup(d).asOption.map(_.model.toString).getOrElse("")
        case nonNodeValue => nonNodeValue.toString
      }.getOrElse("")
    }

    def visit(model: Any): String = visit(model, Map.empty[String, Any])

    def visit(model: Any, extraData: Map[String, Any]): String = {
      val visitorContext = getContext(model)
      new DefaultVisitor(visitorContext, partials, helpers, dataMap ++ extraData).visit(program)
    }

    def inverse(model: Any): String = inverse(model, Map.empty[String, Any])

    def inverse(model: Any, extraData: Map[String, Any]): String = {
      inverseNode.map {
        node =>
          val visitorContext = getContext(model)
          new DefaultVisitor(visitorContext, partials, helpers, dataMap ++ extraData).visit(node)
      }.getOrElse {
        warn("No inverse node found for program: %s".format(program))
        ""
      }
    }

    def lookup(path: String): Option[Any] = {
      HandlebarsGrammar.path(path).map { identifier =>
        context.lookup(identifier, List.empty).asOption.map(_.model)
      }.getOrElse {
        warn("Could not parse path, %s, returning empty string".format(path))
        None
      }
    }

    private def getContext(model: Any) = model match {
      case c:Context[_] => c
      case anyObj => createChild(anyObj, context)
    }
  }
}
