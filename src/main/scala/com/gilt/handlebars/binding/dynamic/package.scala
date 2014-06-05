package com.gilt.handlebars.binding

package object dynamic {
  implicit val bindingFactory = DynamicBinding
  implicit def valueToBinding(v: Any): Binding[Any] = bindingFactory(v)
}