package com.gilt.handlebars.context

/**
 * User: chicks
 * Date: 5/30/13
 */
trait ContextFactory  {
  def createUndefined[T]: Context[T]

  def createRoot[T](model: T): Context[T]

  def createChild[T](model: T, parent: Context[T]): Context[T]
}
