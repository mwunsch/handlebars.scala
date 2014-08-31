package com.gilt.handlebars.scala.binding.dynamic

import java.lang.reflect.Method

import com.gilt.handlebars.scala.binding.{Binding, BindingFactory, FullBinding, VoidBinding}
import com.googlecode.concurrentlinkedhashmap.{ConcurrentLinkedHashMap, Weigher}

class MapWeigher extends Weigher[Map[_, _]] {
  def weightOf(m: Map[_, _]) = m.size
}

object DynamicBindingCache {
  val cache = new ConcurrentLinkedHashMap.Builder[Class[_], Map[String, Method]]()
    .maximumWeightedCapacity(1000)
    .weigher(new MapWeigher)
    .build()

  def getMethods(`class`: Class[_]): Map[String, Method] = {
    val cachedResult = cache.get(`class`)
    if (cachedResult == null) {
      val methodMap = Map(`class`.getMethods.map(m => (m.getName + m.getParameterTypes.length) -> m) : _*)
      cache.put(`class`, methodMap)
      methodMap
    } else cachedResult
  }
}

class DynamicBinding(val data: Any) extends FullBinding[Any] {
  import com.gilt.handlebars.scala.binding.dynamic.DynamicBindingCache.getMethods
  protected val factory = DynamicBinding
  lazy val render = if (isTruthy) data.toString else ""

  lazy val isTruthy = data match {
    case /* UndefinedValue |*/ None | Unit | Nil | null | false => false
    case v: scala.runtime.BoxedUnit => false
    case _ => true
  }
  override def toString = s"DynamicBinding(${data})"
  lazy val isDefined = data match {
    case /* UndefinedValue |*/ None | Unit | null => false
    case _ => true
  }
  def traverse(key: String, args: List[Binding[Any]] = List.empty): Binding[Any] =
    data match {
      case Some(m) => (new DynamicBinding(m)).traverse(key, args)
      case map:Map[_, _] =>
        map.asInstanceOf[Map[String, Any]].get(key) match {
          case Some(value) => new DynamicBinding(value)
          case None => VoidBinding[Any]
        }
      case _ => invoke(key, args)
    }

  lazy val isDictionary = data.isInstanceOf[Map[_, _]]
  lazy val isCollection = data.isInstanceOf[Iterable[_]] && ! isDictionary
  protected def collectionToIterable = data.asInstanceOf[Iterable[Any]]
  protected def dictionaryToIterable = data.asInstanceOf[Map[Any, Any]].toIterable map { case (k, v) => (k.toString, v) }

  protected def invoke(methodName: String, args: List[Binding[Any]] = Nil): Binding[Any] = {
    getMethods(data.getClass).
      get(methodName + args.length).
      map(invoke(_, args)).
      getOrElse(VoidBinding[Any])
  }

  protected def invoke(method: Method, args: List[Binding[Any]]): Binding[Any] = {
//    debug("Invoking method: '%s' with arguments: [%s].".format(method.getName, args.mkString(",")))

    try
      new DynamicBinding(method.invoke(data, args.map(_.get.asInstanceOf[AnyRef]): _*))
    catch {
      case e: java.lang.IllegalArgumentException => VoidBinding
    }
  }

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
