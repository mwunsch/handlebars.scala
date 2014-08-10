package com.gilt.handlebars.scala.binding

trait BindingFactory[T] {
  def apply(model: T): Binding[T]
  def bindPrimitive(s: String): Binding[T]
  def bindPrimitive(s: Int): Binding[T]
  def bindPrimitive(s: Boolean): Binding[T]
  def bindPrimitiveDynamic(v: Any) = {
    v match {
      case i: Int => bindPrimitive(i)
      case b: Boolean => bindPrimitive(b)
      case s => bindPrimitive(s.toString)
    }
  }
}
