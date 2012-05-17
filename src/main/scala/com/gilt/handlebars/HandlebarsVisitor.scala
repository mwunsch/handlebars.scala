package com.gilt.handlebars

import com.gilt.handlebars._

trait Context[T] {
  val context: T

  def invoke(methodName: String): String = {
    //TODO: parameters
    val invocation = getMethod(methodName).map { _.invoke(context) }
    invocation.getOrElse("").toString
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
    case Identifier(ident) => context.invoke(ident)
    case (path: Path) => resolveMustache(path)
    case Mustache(stache, _, escaped) => resolveMustache(stache, escape = escaped)
    case Program(children) => children.map(visit).mkString
    case _ => toString
  }

  def resolveMustache(path: Path, escape: Boolean = true): String = {
    // TODO: resolve tail as well
    val resolution = visit(path.head)
    if (escape) 
      scala.xml.Utility.escape(resolution)
    else
      resolution
  }
}
