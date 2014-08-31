package com.gilt.handlebars.scala.context

import com.gilt.handlebars.scala.binding.{ Binding, VoidBinding }
import com.gilt.handlebars.scala.logging.Loggable
import com.gilt.handlebars.scala.parser.IdentifierNode

class ChildContext[T](val binding: Binding[T], val parent: Context[T]) extends Context[T] {
  def isRoot = false
  def isVoid = !binding.isDefined

  override def toString = s"Child context: binding[$binding] parent[$parent]"
}

class RootContext[T](val binding: Binding[T]) extends Context[T] {
  def isRoot = true
  def isVoid = !binding.isDefined
  def parent = VoidContext[T]
  override def toString = s"Root context: binding[$binding]"
}

trait Context[T] extends Loggable {
  def isRoot: Boolean
  def isVoid: Boolean
  def binding: Binding[T]
  def parent: Context[T]

  def asOption: Option[Context[T]] = binding.asOption map { t => this }

  def render: String = binding.render

  def notEmpty[A](fallback: Context[A]): Context[A] = if (isVoid) fallback else this.asInstanceOf[Context[A]]

  def lookup(path: IdentifierNode, args: Seq[Binding[T]] = Seq()): Context[T] =
    lookup(path.value, args, dictionaryFallbackFlag = false)

  /* mimic "falsy" values of Handlebars.js, plus care about Options
   * @param a
   * @return
   */
  def truthValue = binding.isTruthy

  /**
   * Returns the parent of the provided context, but skips artificial levels in the hierarchy
   * introduced by Iterable, Option, etc.
   */
  def safeParent: Context[T] = {
    if (isRoot || isVoid)
      this
    else if (parent.binding.isDictionary)
      this.parent
    else if (parent.binding.isCollection)
      this.parent.safeParent
    else
      this.parent
  }

  // dictionaryFallbackFlag is work-around for a case in which a context is used to iterate a dictionary
  // It'd be preferable to not create a context for the dictionary (thus preventing the need to skip it), or
  // to capture signal somehow that the binding is being used that way

  @scala.annotation.tailrec
  final def lookup(path: Seq[String], args: Seq[Binding[T]], dictionaryFallbackFlag: Boolean): Context[T] =
    if (path.isEmpty || isVoid) this
    else if (path.head == "." || path.head == "this") {
      if (path.size == 1) this else lookup(path.tail, args, dictionaryFallbackFlag = false)
    } else if (path.head == "..") {
      safeParent.lookup(path.tail, args, dictionaryFallbackFlag = true)
    } else {
      if (dictionaryFallbackFlag && binding.isDictionary) {
        val nextChild = childContext(binding.traverse(path.head, args))
        if (nextChild.isVoid) safeParent.lookup(path, args, dictionaryFallbackFlag = false)
        else nextChild.lookup(path.tail, args, dictionaryFallbackFlag = false)
      } else childContext(binding.traverse(path.head, args)).lookup(path.tail, args, dictionaryFallbackFlag = false)
    }

  def childContext(binding: Binding[T]): Context[T] =
    new ChildContext[T](binding, this)

  def map[R](mapFn: (Context[T], Option[Int]) => R): Iterable[R] =
    if (binding.isCollection) binding.asCollection.zipWithIndex.toSeq.map { tu => mapFn(childContext(tu._1), Some(tu._2)) }
    else Seq(mapFn(this, None))
}

object Context {
  def apply[T](binding: Binding[T]): Context[T] =
    new RootContext(binding)
}

object VoidContext extends Context[Any] {
  def binding = VoidBinding[Any]
  def parent = VoidContext
  def isRoot = false
  def isVoid = true
  override def asOption = None
  override def toString = "Void"
  def apply[T] = this.asInstanceOf[Context[T]]
}
