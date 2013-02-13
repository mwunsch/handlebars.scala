package com.gilt.util

import java.lang.reflect.Method

object GuavaOptionalHelper {

  def invoke[A](context: Any, method: java.lang.reflect.Method, args: List[A]): Option[Any] = {
    val optionalInstance: Object = method.invoke(context, args.map(_.asInstanceOf[AnyRef]): _*)

    try {
      if (isPresent(optionalInstance))
        Some(get(optionalInstance))
      else
        None
    } catch {
      case e:NoSuchMethodException => None
    }
  }

  private def isPresent(instance: Object): Boolean = {
    val meth: Method = instance.getClass.getMethod("isPresent")
    meth.setAccessible(true)
    meth.invoke(instance).asInstanceOf[Boolean]
  }

  private def get(instance: Object): Any = {
    val meth: Method = instance.getClass.getMethod("get")
    meth.setAccessible(true)
    meth.invoke(instance)
  }
}
