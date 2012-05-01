package com.gilt.handlebars

import com.gilt.handlebars._

object Handlebars {

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")): Program = {
    HandlebarsGrammar(delimiters).scan(template)
  }

}

class HandlebarsVisitor[T](context: T) {
  def valueOfMethod(name: String) = {
    try {
      Some(context.getClass.getMethod(name))
    } catch {
      case e: java.lang.NoSuchMethodException => None
    }
  }

  def visit(identifier: Identifier): Option[java.lang.reflect.Method] = {
    valueOfMethod(identifier.value)
  }

  def visit(path: Path): String = {
    val invocation = visit(path.head).map { _.invoke(context) }
    if (!path.tail.isEmpty)
      (new HandlebarsVisitor(invocation.get)).visit(Path(path.tail))
    else invocation.getOrElse("").toString
  }

  def visit(mustache: Mustache): String = {
    mustache match {
      case Mustache(stache, _, true) => scala.xml.Utility.escape(visit(stache))
      case Mustache(stache, _, false) => visit(stache)
    }
  }

  def visit(program: Program): String = program.value.map { node =>
    node match {
      case (m:Mustache) => visit(m)
      case Content(content) => content
      case _ => toString
    }
  }.mkString
}
