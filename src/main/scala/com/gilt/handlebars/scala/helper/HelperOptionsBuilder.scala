package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.{Binding, BindingFactory, VoidBinding}
import com.gilt.handlebars.scala.context.Context
import com.gilt.handlebars.scala.logging.Loggable
import com.gilt.handlebars.scala.parser.{Program, _}
import com.gilt.handlebars.scala.visitor.DefaultVisitor

trait HelperOptions[T] {
  /**
   * Retrieve an argument from the list provided to the helper by its index.
   * @param index the index of the argument
   * @return Option of the argument value, if it exists.
   */
  def argument(index: Int): Binding[T]

  /**
   * Retrieve data provided to the Handlebars template by its key.
   * @param key the key in the map of data provided to Handlebars.apply
   * @return String value of the data retrieved
   */
  def data(key: String): Binding[T]

  /**
   * Evaluates the body of the helper using the provided binding as a context as well as additional data to be combined
   * with the data provided by Handlebars.apply
   * @param binding the context for the body of the helper
   * @param optional extraData data provided by the helper to be used while evaluating the body of the helper.
   * @param optional blockParamsBinding provided by the helper to be used while evaluating the body of the helper.
   * @return String result of evaluating the body.
   */
  def visit(
    binding: Binding[T],
    extraData: Map[String, Binding[T]]=Map(),
    blockParamsBinding: List[Binding[T]]=List()): String

  /**
   * Evaluates the inverse of the body of the helper using the provided binding as a context as well as additional data to
   * be combined with the data provided by Handlebars.apply
   * @param binding the context for the inverse of the body of the helper
   * @param optional extraData data provided by the helper to be used while evaluating the inverse of the body of the helper.
   * @return String result of evaluating the inverse of the body.
   */
  def inverse(binding: Binding[T], extraData: Map[String, Binding[T]]=Map()): String

  /**
   * Look up a path in the the current context. The one in which the helper was called.
   * @param path The path to lookup in the context. e.g., ../name
   * @return Some(binding) where binding is the object that resulted in the lookup. None otherwise.
   */
  def lookup(path: String): Binding[T]

  val dataMap: Map[String, Binding[T]]

  val blockParams: List[String]
}

class HelperOptionsBuilder[T](
    context: Context[T],
    partials: Map[String, Handlebars[T]],
    helpers: Map[String, Helper[T]],
    data: Map[String, Binding[T]],
    program: Node,
    args: Seq[Binding[T]],
    blockParams: List[String],
    outerBlockParamsBinding: Map[String, Binding[T]])(implicit contextFactory: BindingFactory[T]) extends Loggable {

  private val inverseNode: Option[Node] = program match {
    case p:Program => p.inverse
    case b:Block => b.inverse
    case _ => None
  }

  def build: HelperOptions[T] =
    new HelperOptionsImpl(args, data, blockParams)

  private class HelperOptionsImpl(args: Seq[Binding[T]],
                                  val dataMap: Map[String, Binding[T]],
                                  val blockParams: List[String]) extends HelperOptions[T] {

    def argument(index: Int): Binding[T] = {
      args.lift(index) getOrElse VoidBinding[T]
    }

    def data(key: String): Binding[T] = {
      dataMap.get(key).getOrElse(VoidBinding[T])
    }

    def visit(
        binding: Binding[T],
        extraData: Map[String, Binding[T]]=Map(),
        blockParamsBinding: List[Binding[T]]=List()): String = {
      val visitorContext = context.childContext(binding)
      val innerBlockParamsBinding = outerBlockParamsBinding ++ blockParams.zip(blockParamsBinding).toMap
      new DefaultVisitor(
        visitorContext,
        partials,
        helpers,
        dataMap ++ extraData,
        innerBlockParamsBinding).visit(program)
    }

    def inverse(
        binding: Binding[T],
        extraData: Map[String, Binding[T]]=Map()): String = {
      inverseNode.map {
        node =>
          val visitorContext = context.childContext(binding)
          new DefaultVisitor(
            visitorContext,
            partials,
            helpers,
            dataMap ++ extraData,
            Map.empty[String, Binding[T]]).visit(node)
      }.getOrElse {
        warn("No inverse node found for program: %s".format(program))
        ""
      }
    }

    def lookup(path: String): Binding[T] = {
      HandlebarsGrammar.path(path).map { identifier =>
        context.lookup(identifier, Seq()).binding
      }.getOrElse {
        warn("Could not parse path, %s, returning void binding".format(path))
        VoidBinding[T]
      }
    }
  }
}
