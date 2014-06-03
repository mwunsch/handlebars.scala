package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.context.{ Binding, BindingFactory }

class IfHelper[T] extends Helper[T] with Loggable {
  def apply(model: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    if (options.argument(0).isTruthy)
      options.visit(model)
    else
      options.inverse(model)
  }
}
