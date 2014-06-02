package com.gilt.handlebars.context

import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser.{IdentifierNode, Identifier}

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
  val isUndefined = binding.isUndefined

  override def toString = "Child context: model[%s] parent[%s]".format(binding, parent)
}

class RootContext[T](val binding: Binding[T]) extends Context[T] {
  val isRoot = true
  val isUndefined = binding.isUndefined
  val parent = VoidContext[T]
  override def toString = "Root context: model[%s]".format(binding)
}

trait Context[T] extends Loggable {
  val isRoot: Boolean
  val isUndefined: Boolean
  val binding: Binding[T]
  val parent: Context[T]

  def asOption: Option[Context[T]] = binding.filterEmptyLike.toOption map { t => this }

  def render: String = binding.renderString

  def notEmpty[A](fallback: Context[A]): Context[A] = if (isUndefined) fallback else this.asInstanceOf[Context[A]]

  def lookup(path: IdentifierNode, args: List[Any] = List.empty): Context[T] = {
    args match {
      case identifiers: List[_] => {
        val newArgs = identifiers.collect {
          case n: IdentifierNode => lookup(n).binding.get
        }
        lookup(path.value, newArgs)
      }
      case _ =>
        lookup(path.value, args)
    }
  }

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
    if (isRoot || isUndefined)
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
  // to capture signal somehow that the model is being used that way
  def lookup(path: List[String], args: List[Any], dictionaryFallbackFlag: Boolean): Context[T] = {
    path match {
      case Nil => this
      case _ if isUndefined => this
      case ParentIdentifier(p) :: tail =>
        safeParent.lookup(tail, args, true)

      case ThisIdentifier(p) :: tail => if (tail.isEmpty) this else lookup(tail, args)
      case head :: tail => {
        val nextChild = childContext(binding.traverse(head, args))
        if (dictionaryFallbackFlag && nextChild.isUndefined && binding.isDictionary)
          safeParent.lookup(head :: tail, args)
        else
          nextChild.lookup(tail, args)
      }
    }
  }
  def lookup(path: List[String], args: List[Any]): Context[T] = lookup(path, args, false)


  def childContext(model: Binding[T]): Context[T] =
    new ChildContext[T](model, this)

  def isCollection = {
    binding.isCollection
  }

  def map[R]( mapFn: (Context[T], Option[Int]) => R): Iterable[R] = {
    if (binding.isCollection)
      binding.asCollection.zipWithIndex.map {
        case (item, idx) => mapFn(childContext(item), Some(idx))
      }
    else
      Seq(mapFn(this, None))
  }
}

trait ContextFactory[T] {
  def apply(model: T): Context[T]
}

object Context extends ContextFactory[Any] {
  def apply(_model: Any): Context[Any] =
    new RootContext(DynamicBinding(_model))
}
