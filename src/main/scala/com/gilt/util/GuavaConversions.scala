package com.gilt.util

import com.google.common.{base => guava}
import com.google.common.base.{Optional, Predicate, Supplier}

/**
 * In spirit of scala.collection.JavaConversions.
 * Implicit conversion functions for guava package.
 */
object GuavaConversions {

  /**Implicit conversion from guava's <code>Optional</code> to
   * to scala's <code>Option</code>
   */
  implicit def asScalaOption[T](gOpt: Optional[T]): Option[T] = {
    if (gOpt.isPresent) Some(gOpt.get)
    else None
  }

  /**Implicit conversion from scala's <code>Option</code> to
   * to guava's <code>Optional</code>
   */
  implicit def asJavaOptional[T](sOpt: Option[T]): Optional[T] = {
    sOpt.map(Optional.of[T]).getOrElse(Optional.absent())
  }

  implicit def asJavaFunction[T, R](f: T => R): guava.Function[T, R] =
    new guava.Function[T, R] {
      def apply(arg: T): R = f(arg)
    }

  implicit def asScalaFunction[T, R](f: guava.Function[T, R]): (T => R) = {
    f.apply(_)
  }

  implicit def asJavaSupplier[R](s: () => R): Supplier[R] =
    new Supplier[R] {
      def get(): R = s()
    }

  implicit def asScalaFunction0[R](s: Supplier[R]): (() => R) = {
    s.get _
  }

  implicit def asScalaPredicate[T](pred: Predicate[T]): T => Boolean = (t: T) => pred.apply(t)

  implicit def asJavaPredicate[T](pred: T => Boolean): Predicate[T] = new Predicate[T] {
    def apply(input: T) = pred(input)
  }

  /**
   * Can't implicitly convert by-name params, so this method needs to be invoked
   * explicitly.
   */
  def supplier[T](f: => T): Supplier[T] = asJavaSupplier {
    () => f
  }
}
