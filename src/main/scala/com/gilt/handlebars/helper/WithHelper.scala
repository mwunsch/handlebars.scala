package com.gilt.handlebars.helper

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
  def apply(model: Any, options: HelperOptions): String = {
    options.argument(0).map {
      ctx =>
        options.visit(ctx)
    }.getOrElse {
      warn("No context provided for with helper")
      ""
    }
  }
}
