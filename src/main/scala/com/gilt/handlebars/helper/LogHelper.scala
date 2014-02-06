package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable


class LogHelper extends Helper with Loggable {
  def apply(model: Any, options: HelperOptions): String = {
    options.argument(0).map(arg => info(arg.toString))
    ""
  }
}
