package com.gilt.handlebars.scala.binding

trait Binding[T] {
  def isDefined: Boolean
  def get: T
  def getOrElse(default: => T): T
  def render: String

  def isTruthy: Boolean
  def isCollection: Boolean
  def isDictionary: Boolean
  def isPrimitive = ! isCollection && ! isDictionary

  def asOption: Option[Binding[T]] = if (isDefined) Some(this) else None
  def asCollection: Iterable[Binding[T]]
  def asDictionaryCollection: Iterable[(String, Binding[T])]
  def traverse(key: String, args: Seq[Binding[T]] = Seq()): Binding[T] // If traversing a function-like, args are optionally provided
}

object Binding {
  def mapTraverse[T](path: Seq[String], bindings: Map[String, Binding[T]]) = {

    @scala.annotation.tailrec
    def simpleTraverse(p: Seq[String], binding: Binding[T]): Binding[T] =
      if (p.isEmpty) binding else simpleTraverse(p.tail, binding.traverse(p.head))

    def seed = bindings.getOrElse(path.head, VoidBinding[T])
    simpleTraverse(path.tail, seed)
  }
}

trait FullBinding[T] extends Binding[T] {
  protected def data: T
  protected def factory: BindingFactory[T]

  def get = data
  def getOrElse(default: => T) = data

  override def toString = s"FullBinding($data)"

  protected def collectionToIterable: Iterable[T]
  protected def dictionaryToIterable: Iterable[(String, T)]

  def asCollection =
    if (isCollection)
      collectionToIterable.map(factory(_))
    else
      Seq()

  def asDictionaryCollection =
    if (isDictionary)
      dictionaryToIterable map { case (k,v) => (k, factory(v)) }
    else
      Seq()
  override def equals(o: Any) = o match {
    case f: FullBinding[_] =>
      (f.getClass == getClass) && (f.get == data)
    case _ =>
      false
  }
}

object FullBinding {
  def unapply[T](v: FullBinding[T]) = Some(v.get)
}

trait VoidBinding[T] extends Binding[T]  {
  def isDefined = false
  def render = ""
  def traverse(key: String, args: Seq[Binding[T]] = Seq()) = this
  def asCollection = Nil
  def get = throw new RuntimeException("Tried to get value from the void")
  def getOrElse(default: => T) = default
  def isCollection = false
  def asDictionaryCollection = Seq()
  def isDictionary = false
  def isTruthy = false
  override def toString = "VoidBinding"
}

object VoidBinding extends VoidBinding[Any] {
  def apply[T]: VoidBinding[T] = this.asInstanceOf[VoidBinding[T]]
}

