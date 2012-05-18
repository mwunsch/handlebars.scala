package com.gilt.handlebars

import com.gilt.handlebars._

trait Context[T] {
  val context: T

  def invoke(methodName: String): Option[Any] = {
    //TODO: parameters
    getMethod(methodName).flatMap(invoke)
  }

  def invoke(method: java.lang.reflect.Method): Option[Any] = {
    try {
      Some(method.invoke(context))
    } catch {
      case e: java.lang.IllegalArgumentException => None
    }
  }

  def getMethod(name: String) = {
    try {
      Some(context.getClass.getMethod(name))
    } catch {
      case e: java.lang.NoSuchMethodException => None
    }
  }
}

class RootContext[T](val context: T) extends Context[T] 

class ChildContext[T, P](val context: T, val parent: Context[P]) extends Context[T]

class HandlebarsVisitor[T](base: T) {
  val context = new RootContext(base)

  def visit(node: Node): String = node match {
    case Content(content) => content
    case Identifier(ident) => context.invoke(ident).getOrElse("").toString
    case Path(path) => resolvePath(path).flatMap(context.invoke).getOrElse("").toString
    case Mustache(stache, _, escaped) => resolveMustache(stache, escape = escaped)
    case Program(children) => children.map(visit).mkString
    case _ => toString
  }

  def resolveMustache(path: Path, escape: Boolean = true): String = {
    // TODO: resolve tail as well
    val resolution = visit(path)
    if (escape) 
      scala.xml.Utility.escape(resolution)
    else
      resolution
  }

  def resolvePath(value: List[Identifier]): Option[java.lang.reflect.Method]  = {
    value.foldLeft(None: Option[java.lang.reflect.Method]) { (option, identifier) =>
      context.getMethod(identifier.value)
    }
  }
}
