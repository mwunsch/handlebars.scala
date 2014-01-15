package com.gilt.handlebars.helper

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.logging.Loggable


class IfHelper extends Helper with ClassCacheableContextFactory with Loggable {
  def apply(context: Context[Any], options: HelperOptions): String = {
    options.args.headOption.map {
      firstArg =>
        val argAsContext = createRoot(firstArg)
        if(argAsContext.truthValue) {
          options.visit(HelperContext(context))
        } else {
          options.inverse.map(f => f(HelperContext(context))).getOrElse("")
        }
    }.getOrElse("")
  }
}
