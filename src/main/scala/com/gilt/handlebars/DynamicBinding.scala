package com.gilt.handlebars

import com.gilt.handlebars.binding.{BindingFactory,Binding}

trait BindingPackage[T] {
  implicit def valueToBinding(v: T): Binding[T] = bindingFactory(v)
  implicit val bindingFactory: BindingFactory[T]
}