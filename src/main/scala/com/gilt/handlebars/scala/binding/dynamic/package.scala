package com.gilt.handlebars.scala.binding

package object dynamic {
  implicit val bindingFactory = DynamicBinding
  implicit def valueToBinding(v: Any): Binding[Any] = bindingFactory(v)
}