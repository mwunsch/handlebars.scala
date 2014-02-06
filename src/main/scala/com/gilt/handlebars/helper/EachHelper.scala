package com.gilt.handlebars.helper

import com.gilt.handlebars.logging.Loggable

class EachHelper extends Helper with Loggable {
  def apply(model: Any, options: HelperOptions): String = {
    options.argument(0).map {
      case m:Map[_,_] => m.zipWithIndex.map {
        case ((key, value), idx) => options.visit(value, Map("key" -> key, "index" -> idx))
      }.mkString
      case i:Iterable[_] => i.zipWithIndex.map {
        case (value, idx) => options.visit(value, Map("index" -> idx))
      }.mkString
      case _ => {
        warn("Could not iterate over argument for {{#each}}")
        ""
      }
    }.getOrElse("")
  }
}
