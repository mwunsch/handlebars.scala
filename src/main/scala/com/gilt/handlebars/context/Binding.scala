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
  def asDictionaryCollection: Iterable[(String, Binding[T])]
  def traverse(key: String, args: List[Binding[T]] = List.empty): Binding[T] // If traversing a function-like, args are optionally provided
  def noneIfUndefined: Option[Binding[T]] = if (isUndefined) None else Some(this)
}

object Binding {
  def mapTraverse[T](path: List[String], bindings: Map[String, Binding[T]]) = {
    def simpleTraverse(p: List[String], binding: Binding[T]): Binding[T] = {
      p match {
        case Nil => binding
        case head :: tail => simpleTraverse(tail, binding.traverse(head))
      }
    }

    val seed = bindings.get(path.head) getOrElse { VoidBinding[T] }
    simpleTraverse(path.tail, seed)
  }
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
  def traverse(key: String, args: List[Binding[T]] = List.empty) = this
  lazy val asCollection = Seq()
  def get = throw new RuntimeException("Tried to get value where not defined")
  def isCollection = false
  def asDictionaryCollection = Seq()
  def isDictionary = false
  def isTruthy = false
  override def toString = "VoidBinding"
}

object VoidBinding extends VoidBinding[Any] {
  def apply[T]: VoidBinding[T] = this.asInstanceOf[VoidBinding[T]]
}

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
