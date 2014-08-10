package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.logging.Loggable
import com.gilt.handlebars.scala.binding.Binding
import com.gilt.handlebars.scala.binding.BindingFactory

class LogHelper[T] extends Helper[T] with Loggable {
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    info(options.argument(0).render)
    ""
  }
}
