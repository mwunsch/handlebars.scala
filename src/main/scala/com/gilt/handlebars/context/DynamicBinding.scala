package com.gilt.handlebars.context

import com.gilt.handlebars.logging.Loggable
import java.lang.reflect.Method


case class DynamicBinding(val data: Any) extends FullBinding[Any] with Loggable {
  override def toOption = if (data == null) None else Some(data)
  lazy val renderString = if (isTruthy) data.toString else ""

  lazy val isTruthy = data match {
    case /* UndefinedValue |*/ None | false | Nil | null | "" => false
    case _ => true
  }
  override def toString = s"DynamicBinding(${data})"
  lazy val isDictionary = data.isInstanceOf[Map[_, _]]
  lazy val isCollection = data.isInstanceOf[Iterable[_]] && ! isDictionary
  val isUndefined = false
  protected lazy val isValueless = data match {
    case /* UndefinedValue |*/ None | null => true
    case _ => false
  }
  def traverse(key: String, args: List[Any]): Binding[Any] =
    data match {
      case Some(m) => (new DynamicBinding(m)).traverse(key, args)
      case map:Map[_, _] =>
        map.asInstanceOf[Map[String, Any]].get(key) match {
          case Some(value) => new DynamicBinding(value)
          case None => VoidBinding[Any]
        }

      case _ => invoke(key, args)
    }


  lazy val asCollection =
    if (isCollection)
      data.asInstanceOf[Iterable[Any]].map(DynamicBinding(_))
    else
      Seq(this)

  protected def invoke(methodName: String, args: List[Any] = Nil): Binding[Any] = {
    methods.
      get(methodName + args.length).
      map(invoke(_, args)).
      getOrElse(VoidBinding[Any])
  }

  protected def invoke(method: Method, args: List[Any]): Binding[Any] = {
    debug("Invoking method: '%s' with arguments: [%s].".format(method.getName, args.mkString(",")))

    try
      DynamicBinding(method.invoke(data, args.map(_.asInstanceOf[AnyRef]): _*))
    catch {
      case e: java.lang.IllegalArgumentException => VoidBinding
    }
  }

  // TODO - move cache to shared structure?
  lazy protected val methods: Map[String, Method] = {
     Map(data.getClass.getMethods.map(m => (m.getName + m.getParameterTypes.length) -> m) : _*)
  }

  protected def isPrimitiveType(obj: Any) = obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
    obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]

}

object VoidContext extends Context[Any] {
  val binding = VoidBinding[Any]
  val parent = VoidContext
  val isRoot = false
  val isUndefined = true // SMELL: this attribute stinks
  override def asOption = None
  def apply[T] = this.asInstanceOf[Context[T]]
  override def toString = "Void"
}
