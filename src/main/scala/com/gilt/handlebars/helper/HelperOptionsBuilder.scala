package com.gilt.handlebars.helper

import com.gilt.handlebars.parser._
import com.gilt.handlebars.context.{ContextFactory, Context, Binding}
import com.gilt.handlebars.Handlebars
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser.Program
import com.gilt.handlebars.visitor.DefaultVisitor
import com.gilt.handlebars.context.DynamicBinding

trait HelperOptions {
  /**
   * Retrieve an argument from the list provided to the helper by its index.
   * @param index the index of the argument
   * @return Option of the argument value, if it exists.
   */
  def argument(index: Int): Option[Any]

  /**
   * Retrieve data provided to the Handlebars template by its key.
   * @param key the key in the map of data provided to Handlebars.apply
   * @return String value of the data retrieved
   */
  def data(key: String): String

  /**
   * Evaluates the body of the helper using the provided model as a context.
   * @param model the context for the body of the helper
   * @return String result of evaluating the body.
   */
  def visit(model: Any): String

  /**
   * Evaluates the body of the helper using the provided model as a context as well as additional data to be combined
   * with the data provided by Handlebars.apply
   * @param model the context for the body of the helper
   * @param extraData data provided by the helper to be used while evaluating the body of the helper.
   * @return String result of evaluating the body.
   */
  def visit(model: Any, extraData: Map[String, Any]): String

  /**
   * Evaluate the inverse of body of the helper using the provided model as a context.
   * @param model the context for the inverse of the body of the helper
   * @return String result of evaluating the body.
   */
  def inverse(model: Any): String

  /**
   * Evaluates the inverse of the body of the helper using the provided model as a context as well as additional data to
   * be combined with the data provided by Handlebars.apply
   * @param model the context for the inverse of the body of the helper
   * @param extraData data provided by the helper to be used while evaluating the inverse of the body of the helper.
   * @return String result of evaluating the inverse of the body.
   */
  def inverse(model: Any, extraData: Map[String, Any]): String

  /**
   * Look up a path in the the current context. The one in which the helper was called.
   * @param path The path to lookup in the context. e.g., ../name
   * @return Some(model) where model is the object that resulted in the lookup. None otherwise.
   */
  def lookup(path: String): Option[Any]
}

class HelperOptionsBuilder(context: Context[Any],
                           partials: Map[String, Handlebars],
                           helpers: Map[String, Helper],
                           data: Map[String, Any],
                           program: Node,
                           params: List[ValueNode])(implicit contextFactory: ContextFactory[Any]) extends Loggable {

  private val args = params.map {
    case i:IdentifierNode =>
      // 1. Look in the Context
      context.lookup(i).asOption.flatMap(_.binding.toOption).orElse {
        // 2. Check the global data, but treat it as a context in case the path is 'foo.bar'
        contextFactory(data).lookup(i).asOption.flatMap(_.binding.toOption)
      }.getOrElse {
        // 3. Give up, path wasn't found anywhere
        warn("Path not found for helper: %s".format(i.string))
        ""
      }
    case m: Binding[_] => m.toOption getOrElse ""
    case a => a
  }

  private val inverseNode: Option[Node] = program match {
    case p:Program => p.inverse
    case b:Block => b.inverse
    case _ => None
  }

  def build: HelperOptions = {
    new HelperOptionsImpl(args, data)
  }

  private class HelperOptionsImpl(args: List[Any],
                                  dataMap: Map[String, Any]) extends HelperOptions {

    def argument(index: Int): Option[Any] = {
      args.lift(index)
    }

    def data(key: String): String = {
      dataMap.get(key).map {
        case d:DataNode => contextFactory(dataMap).lookup(d).asOption.map(_.binding.renderString).getOrElse("")
        case nonNodeValue => nonNodeValue.toString
      }.getOrElse("")
    }

    def visit(data: Any): String = visit(data, Map.empty[String, Any])

    def visit(data: Any, extraData: Map[String, Any]): String = {
      val visitorContext = childContext(data)
      new DefaultVisitor(visitorContext, partials, helpers, dataMap ++ extraData).visit(program)
    }

    def inverse(model: Any): String = inverse(model, Map.empty[String, Any])

    def inverse(model: Any, extraData: Map[String, Any]): String = {
      inverseNode.map {
        node =>
          val visitorContext = childContext(model)
          new DefaultVisitor(visitorContext, partials, helpers, dataMap ++ extraData).visit(node)
      }.getOrElse {
        warn("No inverse node found for program: %s".format(program))
        ""
      }
    }

    def lookup(path: String): Option[Any] = {
      HandlebarsGrammar.path(path).map { identifier =>
        context.lookup(identifier, List.empty).asOption.flatMap(_.binding.toOption)
      }.getOrElse {
        warn("Could not parse path, %s, returning empty string".format(path))
        None
      }
    }

    private def childContext(data: Any) =
      context.childContext(DynamicBinding(data))
  }
}
