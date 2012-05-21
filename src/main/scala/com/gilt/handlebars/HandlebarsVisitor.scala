package com.gilt.handlebars

import com.gilt.handlebars._

trait Context[T] {
  val context: T

  def invoke[A](methodName: String, args: List[A] = Nil): Option[Any] = {
    //TODO: parameters
    getMethod(methodName, args).flatMap(invoke(_, args.map(_.asInstanceOf[java.lang.Object])))
  }

  def invoke(method: java.lang.reflect.Method, args: Seq[java.lang.Object]): Option[Any] = {
    try {
      Some(method.invoke(context, args: _*))
    } catch {
      case e: java.lang.IllegalArgumentException => None
    }
  }

  def getMethod[A](name: String, args: Seq[A] = Nil) = {
    try {
      if (args.isEmpty)
        Some(context.getClass.getMethod(name))
      else
        Some(context.getClass.getMethod(name, args.map(_.getClass): _*))
    } catch {
      case e: java.lang.NoSuchMethodException => None
    }
  }

}

case class RootContext[T](context: T) extends Context[T] 

case class ChildContext[T, P](context: T, parent: Context[P]) extends Context[T]

object HandlebarsVisitor {
  def apply[T](base: T) = new HandlebarsVisitor(new RootContext(base))
}

class HandlebarsVisitor[T](context: Context[T]) {
  def visit(node: Node): String = node match {
    case Content(content) => content
    case Identifier(ident) => context.invoke(ident).getOrElse("").toString
    case Path(path) => resolvePath(path).getOrElse(new RootContext("")).context.toString
    case Comment(_) => ""
    case Partial(partial) => compilePartial(partial).getOrElse("")
    case Mustache(stache, params, escaped) => resolveMustache(stache, params, escape = escaped)
    case Section(stache, value, inverted) => renderSection(stache.value, value, inverted).getOrElse("")
    case Program(children) => children.map(visit).mkString
    case _ => toString
  }

  def resolveMustache(path: Path, parameters: List[Path], escape: Boolean = true): String = {
    val args = parameters.flatMap(param => resolvePath(param.value))
    val lookup = resolvePath(path.value, args)
    val resolution = lookup.getOrElse(new RootContext("")).context.toString
    if (escape) 
      scala.xml.Utility.escape(resolution)
    else
      resolution
  }

  def resolvePath(list: List[Identifier], args: List[Context[Any]] = Nil): Option[Context[Any]] = {
    list.foldLeft(None: Option[Context[Any]]) { (someContext, identifier) =>
      someContext.orElse(Some(context)).flatMap { aContext =>
        if (identifier.value == "..") {
          aContext match {
            case ChildContext(_, parent) => Some(parent)
            case _ => None
          }
        } else {
          aContext.invoke(identifier.value, args.map(_.context)).map(new ChildContext(_, aContext))
        }
      }
    }
  }

  def renderSection(path: Path, program: Program, inverted: Boolean = false): Option[String] = {
    resolvePath(path.value).map { block =>
      val visitors = block.context match {
        // TODO: Lambdas
        case list:Iterable[_] => list.map(i => new HandlebarsVisitor(new ChildContext(i, block)))
        case value:Some[_] => Seq(new HandlebarsVisitor(new ChildContext(value.get, block)))
        case _ => Seq(new HandlebarsVisitor(block))
      }
      visitors.map(_.visit(program)).mkString
    } orElse { if (inverted) Some(visit(program)) else None }
  }

  def compilePartial(path: Path): Option[String] = {
    resolvePath(path.value).map { context =>
      this visit HandlebarsGrammar().scan(context.context.toString)
    }
  }
}
