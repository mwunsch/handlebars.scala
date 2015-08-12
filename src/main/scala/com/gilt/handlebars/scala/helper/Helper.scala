package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.binding.Binding
import com.gilt.handlebars.scala.binding.BindingFactory

/**
 * The Scandlebars definition of a handlebars helper.
 *
 * The recommended fashion in which to define a helper is as follows. HelperOptions.lookup returns an Option which will
 * evaluate to an empty string if the lookup does not yield a result.
 *
 * {{{
 *  val fullNameHelper = Helper {
 *    (binding, options) =>
 *      "%s %s".format(options.lookup("firstName"), options.lookup("lastName"))
 *  }
 * }}}
 *
 * It may be likely that you can predict the type of binding in practice. You could attempt to cast the context binding to
 * the type you expect. This runs the risk of throwing a ClassCastException while rendering the template, which may be
 * desirable. Use cautiously.
 *
 * {{{
 *  val fullNameHelper = Helper {
 *    (binding, options) =>
 *      val person = binding.asInstanceOf[Person]
 *      "%s %s".format(person.firstName, person.lastName)
 *  }
 * }}}
 *
 * See [[com.gilt.handlebars.scala.helper.HelperOptions]] for all of the options available.
 *
 */
trait Helper[T] {
  /**
   * Executes the helper
   * @param binding the context object for the helper
   * @param options the options or utility methods for the helper.
   * @return String result after evaluating the helper
   */
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit contextFactory: BindingFactory[T]): String
}

object Helper {
  def apply[T](f: ((Binding[T], HelperOptions[T]) => String)): Helper[T] = {
    new Helper[T] {
      def apply(binding: Binding[T], options: HelperOptions[T])(implicit contextFactory: BindingFactory[T]): String = f(binding, options)
    }
  }

  def defaultHelpers[T]: Map[String, Helper[T]] = Map (
    "with" -> new WithHelper[T],
    "if" -> new IfHelper[T],
    "unless" -> new IfHelper[T](inverse = true),
    "each" -> new EachHelper[T],
    "log" -> new LogHelper[T]
  )
}

class StaticHelper[T](staticValue: T) extends Helper[T] {
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    staticValue.toString
  }
}
