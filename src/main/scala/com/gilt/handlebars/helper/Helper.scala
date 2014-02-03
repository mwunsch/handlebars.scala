package com.gilt.handlebars.helper

import com.gilt.handlebars.context.ClassCacheableContextFactory

/**
 * The Scandlebars definition of a handlebars helper.
 *
 * The recommended fashion in which to define a helper is as follows. HelperOptions.lookup returns an Option which will
 * evaluate to an empty string if the lookup does not yield a result.
 *
 * {{{
 *  val fullNameHelper = Helper {
 *    (model, options) =>
 *      "%s %s".format(options.lookup("firstName"), options.lookup("lastName"))
 *  }
 * }}}
 *
 * It may be likely that you can predict the type of model in practice. You could attempt to cast the context model to
 * the type you expect. This runs the risk of throwing a ClassCastException while rendering the template, which may be
 * desirable. Use cautiously.
 *
 * {{{
 *  val fullNameHelper = Helper {
 *    (model, options) =>
 *      val person = model.asInstanceOf[Person]
 *      "%s %s".format(person.firstName, person.lastName)
 *  }
 * }}}
 *
 * See [[com.gilt.handlebars.helper.HelperOptions]] for all of the options available.
 *
 */
trait Helper {
  /**
   * Executes the helper
   * @param model the context object for the helper
   * @param options the options or utility methods for the helper.
   * @return String result after evaluating the helper
   */
  def apply(model: Any, options: HelperOptions): String
}

object Helper extends ClassCacheableContextFactory {
  def apply(f: ((Any, HelperOptions) => String)): Helper = {
    new Helper {
      def apply(model: Any, options: HelperOptions): String = f(model, options)
    }
  }

  lazy val defaultHelpers: Map[String, Helper] = Map (
    "with" -> new WithHelper,
    "if" -> new IfHelper,
    "each" -> new EachHelper,
    "log" -> new LogHelper
  )
}

class StaticHelper(staticValue: Any) extends Helper {
  def apply(model: Any, options: HelperOptions): String = {
    staticValue.toString
  }
}
