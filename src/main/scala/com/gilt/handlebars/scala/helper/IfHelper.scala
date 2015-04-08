package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.logging.Loggable
import com.gilt.handlebars.scala.binding.{ Binding, BindingFactory }

class IfHelper[T] extends Helper[T] with Loggable {
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    if (options.argument(0).isTruthy)
      options.visit(binding)
    else
      if (options.hasInverse) options.inverse(binding)
      else ""
  }
}
