package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.context.Context

class EachHelper extends Helper with Loggable {
  def apply(context: Context[Any], options: HelperOptions): String = {
    options.args.headOption.map {
      case m:Map[_,_] => m.zipWithIndex.map {
        case ((key, value), idx) => options.visit(HelperContext(value, Map("key" -> key, "index" -> idx)))
      }.mkString
      case i:Iterable[_] => i.zipWithIndex.map {
        case (value, idx) => options.visit(HelperContext(value, Map("index" -> idx)))
      }.mkString
      case _ => {
        warn("Could not iterate over argument for {{#each}}")
        ""
      }
    }.getOrElse("")
  }
}
