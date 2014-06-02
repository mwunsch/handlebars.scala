package com.gilt.handlebars.context

import java.lang.reflect.Method
import com.gilt.handlebars.logging.Loggable
import java.util.concurrent.ConcurrentHashMap

/**
 * User: chicks
 * Date: 5/30/13
 * Time: 11:38 AM
 */
object ClassCacheableContext {
  val cache = new ConcurrentHashMap[Class[_], Map[String, Method]]
}
trait ClassCacheableContextFactory extends ContextFactory { factory =>
  def createUndefined[T]: Context[T] = {
    new Context[T] with ClassCacheableContext[T] {
      override val isRoot = false
      override val isUndefined = true
      val contextFactory = factory
      val model: T = null.asInstanceOf[T]
      val parent: Context[T] = null.asInstanceOf[Context[T]]
    }
  }

  def createRoot[T](_model: T): Context[T] = {
    new Context[T] with ClassCacheableContext[T] {
      val model: T = _model
      val isUndefined: Boolean = false
      val contextFactory = factory
      val isRoot: Boolean = true
      val parent: Context[T] = createUndefined
    }
  }

  def createChild[T](_model: T, _parent: Context[T]): Context[T] = {
    new Context[T] with ClassCacheableContext[T] {
      val model: T = _model
      val isUndefined: Boolean = false
      val contextFactory = factory
      val isRoot: Boolean = false
      val parent: Context[T] = _parent
    }
  }
}

trait ClassCacheableContext[+T]
  extends CacheableContext[T, Class[_], Map[String, Method]]
  with Loggable
{
  val model: T

  override val cache = ClassCacheableContext.cache

  override protected def getMethods(clazz: Class[_]): Map[String, Method] = {
    Option(cache.get(clazz)).getOrElse {
      val methods = super.getMethods(clazz)
      cache.put(clazz, methods)
      methods
    }
  }
}
