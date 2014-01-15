package com.gilt.handlebars.helper

import com.gilt.handlebars.context.Context
import com.gilt.handlebars.logging.Loggable

/**
 * with Helper
 *
 * Usage:
 *
 * {{with withContext}} {{!body}} {{/with}}
 *
 * Any mustache will be with respect to withContext not the context in which the helper was called.
 */
class WithHelper extends Helper with Loggable {
  def apply(context: Context[Any], options: HelperOptions): String = {
    options.args.headOption.map {
      ctx =>
        options.visit(HelperContext(ctx))
    }.getOrElse {
      warn("No context provided for with helper")
      ""
    }
  }
}
