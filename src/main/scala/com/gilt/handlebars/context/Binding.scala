package com.gilt.handlebars.context

trait Binding[T] {
  val isUndefined: Boolean
  def filterEmptyLike: Binding[T]
  protected def isValueless: Boolean
  def get: T
  def toOption: Option[T]
  def renderString: String
  def isTruthy: Boolean
  def isCollection: Boolean
  def isDictionary: Boolean
  def isPrimitive = ! isCollection && ! isDictionary
  def asCollection: Iterable[Binding[T]]
  def traverse(key: String, args: List[Any]): Binding[T] // If traversing a function-like, args are optionally provided
}

trait FullBinding[T] extends Binding[T] {
  protected val data: T
  if (data.isInstanceOf[Binding[_]]) {
    throw new Exception("Bug! You tried to wrap a binding with a binding. Don't do that!")
  }
  def toOption: Option[T] = Some(data)
  def get = data
  override def toString = s"FullBinding(${data})"
  def filterEmptyLike =
    if (isValueless)
      VoidBinding[T]
    else
      this
}

trait VoidBinding[T] extends Binding[T]  {
  val isUndefined = true
  def toOption: Option[T] = None
  def renderString = ""
  def isValueless = true
  def filterEmptyLike = this
  def traverse(key: String, args: List[Any]) = this
  lazy val asCollection = Seq()
  def get = throw new RuntimeException("Tried to get value where not defined")
  def isCollection = false
  def isDictionary = false
  def isTruthy = false
  override def toString = "VoidBinding"
}

object VoidBinding extends VoidBinding[Any] {
  def apply[T]: VoidBinding[T] = this.asInstanceOf[VoidBinding[T]]
}
