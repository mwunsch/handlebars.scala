package com.gilt.handlebars.helper

import com.gilt.handlebars.context.ClassCacheableContextFactory
import com.gilt.handlebars.logging.Loggable


class IfHelper extends Helper with ClassCacheableContextFactory with Loggable {
  def apply(model: Any, options: HelperOptions): String = {
    options.argument(0).map {
      firstArg =>
        val argAsContext = createRoot(firstArg)
        if(argAsContext.truthValue) {
          options.visit(model)
        } else {
          options.inverse(model)
        }
    }.getOrElse("")
  }
}
