package com.gilt.handlebars.scala.binding

import scala.language.implicitConversions

package object dynamic {
  implicit val bindingFactory = DynamicBinding
  implicit def valueToBinding(v: Any): Binding[Any] = bindingFactory(v)
}