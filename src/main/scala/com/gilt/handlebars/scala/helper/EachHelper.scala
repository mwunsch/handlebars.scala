package com.gilt.handlebars.scala.helper

import com.gilt.handlebars.scala.binding.{Binding, BindingFactory}

class EachHelper[T] extends Helper[T] {
  def apply(binding: Binding[T], options: HelperOptions[T])(implicit contextFactory: BindingFactory[T]): String = {
    val arg0 = options.argument(0)
    if (arg0.isDictionary)
      arg0.asDictionaryCollection.zipWithIndex.map {
        case ((key, value), idx) =>
          options.visit(value,
            Map(
              "key" -> contextFactory.bindPrimitive(key),
              "index" -> contextFactory.bindPrimitive(idx)),
            List(
              value,
              contextFactory.bindPrimitive(key)))
      }.mkString
    else if (arg0.isCollection)
      arg0.asCollection.zipWithIndex.map {
        case (value, idx) =>
          options.visit(value,
            Map[String, Binding[T]](
              "index" -> contextFactory.bindPrimitive(idx)),
            List[Binding[T]](
              value,
              contextFactory.bindPrimitive(idx)))
      }.mkString

    else {
//      warn("Could not iterate over argument for {{#each}}")
      ""
    }
  }
}
