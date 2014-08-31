package com.gilt.handlebars.scala.binding.dynamic

import java.lang.reflect.Method
import com.gilt.handlebars.scala.binding.{Binding, BindingFactory, FullBinding, VoidBinding}
import com.gilt.handlebars.scala.logging.Loggable
import java.lang.reflect.Modifier

object DynamicBindingCache {
  private val exclude = Seq(
    "canEqual",
    "unapply",
    "hashCode",
    "productElement",
    "productIterator",
    "productArity",
    "productPrefix",
    "wait",
    "toString",
    "getClass",
    "notify",
    "notifyAll"
  )

  private def lookupMethods(clazz: Class[_]) = clazz.getMethods
    .filterNot { m => exclude.contains(m.getName) }
    .filterNot { m => m.getName.indexOf("$") >= 0 }
    .filterNot { m => m.getReturnType.toString == "void" } // we don't want side effects in rendering
    .filterNot { m => (m.getModifiers & Modifier.PUBLIC) == 0 }
    .map(m => (m.getName + m.getParameterTypes.length) -> m)
    .toMap

  import scala.collection.JavaConverters._
  private val methodsCache = new java.util.concurrent.ConcurrentHashMap[Class[_], Map[String, Method]].asScala

  def getMethods(clazz: Class[_]): Map[String, Method] = methodsCache.get(clazz) match {
    case Some(me) => me
    case None =>
      methodsCache(clazz) = lookupMethods(clazz)
      methodsCache(clazz)
  }
}


class DynamicBinding(val data: Any) extends FullBinding[Any] with Loggable {
  override protected def factory = DynamicBinding
  def render = if (isTruthy) data.toString else ""

  def isTruthy = data match {
    case /* UndefinedValue |*/ None | Unit | Nil | null | false => false
    case _: scala.runtime.BoxedUnit => false
    case _ => true
  }
  override def toString = s"DynamicBinding($data)"
  override def isDefined = data match {
    case /* UndefinedValue |*/ None | Unit | null => false
    case _ => true
  }

  @scala.annotation.tailrec
  final def traverse(key: String, args: Seq[Binding[Any]] = Nil): Binding[Any] = data match {
    case Some(m) => new DynamicBinding(m).traverse(key, args)
    case _: Map[_, _] => data.asInstanceOf[Map[String, _]]
      .get(key)
      .map(new DynamicBinding(_))
      .getOrElse(VoidBinding)
    case _ => DynamicBindingCache
      .getMethods(data.getClass)
      .get(key + args.length)
      .map { method =>
      try {
        if (args.isEmpty) new DynamicBinding(method.invoke(data))
        else new DynamicBinding(method.invoke(data, args.map(_.get).asInstanceOf[Seq[AnyRef]]: _*))
      }
      catch {
        case e: java.lang.IllegalArgumentException =>
          sys.error(s"method $key with args $args invoked on $data failed with cause: ${e.getStackTrace.mkString("\n")}")
      }
    }.getOrElse(VoidBinding)
  }

  def isDictionary = data.isInstanceOf[Map[_, _]]
  def isCollection = data.isInstanceOf[Iterable[_]] && ! isDictionary
  protected def collectionToIterable = data.asInstanceOf[Iterable[Any]]
  protected def dictionaryToIterable = data.asInstanceOf[Map[Any, Any]].toIterable map { case (k, v) => (k.toString, v) }
  protected def isPrimitiveType(obj: Any) = obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
    obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]
}

object DynamicBinding extends BindingFactory[Any] {
  def apply(model: Any): Binding[Any] =
    new DynamicBinding(model)

  def bindPrimitive(model: String) = apply(model)
  def bindPrimitive(model: Boolean) = apply(model)
  def bindPrimitive(model: Int) = apply(model)
}
