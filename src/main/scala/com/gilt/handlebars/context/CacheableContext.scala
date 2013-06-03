package com.gilt.handlebars.context

import java.util.concurrent.ConcurrentHashMap

/**
 * User: chicks
 * Date: 5/30/13
 */
trait CacheableContext[+T, K, V] extends Context[T] {
  val cache = new ConcurrentHashMap[K, V]
}
