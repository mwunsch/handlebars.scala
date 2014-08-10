package com.gilt.handlebars.scala.context

import com.gilt.handlebars.scala.binding.{Binding, VoidBinding}
import com.gilt.handlebars.scala.logging.Loggable
import com.gilt.handlebars.scala.parser.IdentifierNode

object ParentIdentifier {
  def unapply(s: String): Option[String] = {
    if ("..".equals(s)) Some(s) else None
  }
}

object ThisIdentifier {
  def unapply(s: String): Option[String] = {
    if (".".equals(s) || "this".equals(s)) Some(s) else None
  }
}


class ChildContext[T](val binding: Binding[T], val parent: Context[T]) extends Context[T] {
  val isRoot = false
  val isVoid = ! binding.isDefined

  override def toString = "Child context: binding[%s] parent[%s]".format(binding, parent)
}

class RootContext[T](val binding: Binding[T]) extends Context[T] {
  val isRoot = true
  val isVoid = ! binding.isDefined
  val parent = VoidContext[T]
  override def toString = "Root context: binding[%s]".format(binding)
}

trait Context[T] extends Loggable {
  val isRoot: Boolean
  val isVoid: Boolean
  val binding: Binding[T]
  val parent: Context[T]

  def asOption: Option[Context[T]] = binding.asOption map { t => this }

  def render: String = binding.render

  def notEmpty[A](fallback: Context[A]): Context[A] = if (isVoid) fallback else this.asInstanceOf[Context[A]]

  def lookup(path: IdentifierNode, args: List[Binding[T]] = List.empty): Context[T] =
    lookup(path.value, args)


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
  def lookup(path: List[String], args: List[Binding[T]], dictionaryFallbackFlag: Boolean): Context[T] = {
    path match {
      case Nil => this
      case _ if isVoid => this
      case ParentIdentifier(p) :: tail =>
        safeParent.lookup(tail, args, true)

      case ThisIdentifier(p) :: tail => if (tail.isEmpty) this else lookup(tail, args)
      case head :: tail => {
        val nextChild = childContext(binding.traverse(head, args))
        if (dictionaryFallbackFlag && nextChild.isVoid && binding.isDictionary)
          safeParent.lookup(head :: tail, args)
        else
          nextChild.lookup(tail, args)
      }
    }
  }
  def lookup(path: List[String], args: List[Binding[T]]): Context[T] = lookup(path, args, false)

  def childContext(binding: Binding[T]): Context[T] =
    new ChildContext[T](binding, this)

  def map[R]( mapFn: (Context[T], Option[Int]) => R): Iterable[R] = {
    if (binding.isCollection)
      binding.asCollection.zipWithIndex.map {
        case (item, idx) => mapFn(childContext(item), Some(idx))
      }
    else
      Seq(mapFn(this, None))
  }
}

object Context {
  def apply[T](binding: Binding[T]): Context[T] =
    new RootContext(binding)
}

object VoidContext extends Context[Any] {
  val binding = VoidBinding[Any]
  val parent = VoidContext
  val isRoot = false
  val isVoid = true
  override def asOption = None
  override def toString = "Void"
  def apply[T] = this.asInstanceOf[Context[T]]
}
