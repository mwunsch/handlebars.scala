package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.context.Binding
import com.gilt.handlebars.context.DynamicBinding


class IfHelper extends Helper with Loggable {
  def apply(model: Any, options: HelperOptions): String = {
    options.argument(0).map {
      firstArg =>
        if(DynamicBinding(firstArg).isTruthy) {
          options.visit(model)
        } else {
          options.inverse(model)
        }
    }.getOrElse("")
  }
}
