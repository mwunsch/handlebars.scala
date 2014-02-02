package com.gilt.handlebars.helper

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}

trait Helper {
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
