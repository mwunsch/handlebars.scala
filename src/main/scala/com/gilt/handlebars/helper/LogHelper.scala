package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.context.Binding
import com.gilt.handlebars.context.BindingFactory

class LogHelper[T] extends Helper[T] with Loggable {
  def apply(model: Binding[T], options: HelperOptions[T])(implicit c: BindingFactory[T]): String = {
    info(options.argument(0).renderString)
    ""
  }
}
