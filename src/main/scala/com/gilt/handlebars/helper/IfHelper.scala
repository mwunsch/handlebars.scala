package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.binding.{ Binding, BindingFactory }

class IfHelper[T] extends Helper[T] with Loggable {
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    if (options.argument(0).isTruthy)
      options.visit(binding)
    else
      options.inverse(binding)
  }
}
