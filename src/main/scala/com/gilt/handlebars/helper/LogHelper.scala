package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.context.Context


class LogHelper extends Helper with Loggable {
  def apply(context: Context[Any], options: HelperOptions): String = {
    options.args.headOption.map(arg => info(arg.toString))
    ""
  }
}
