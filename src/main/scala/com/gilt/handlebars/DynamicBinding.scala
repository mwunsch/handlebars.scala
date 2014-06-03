package com.gilt.handlebars

import com.gilt.handlebars.context.{BindingFactory,Binding}

trait BindingPackage[T] {
  implicit def valueToBinding(v: T): Binding[T] = bindingFactory(v)
  implicit val bindingFactory: BindingFactory[T]
}

object DynamicBinding extends BindingPackage[Any] {
  implicit val bindingFactory = context.DynamicBinding
}