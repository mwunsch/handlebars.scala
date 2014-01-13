package com.gilt.handlebars.context

import com.gilt.handlebars.logging.Loggable
import java.lang.reflect.Method
import com.gilt.handlebars.parser.{IdentifierNode, Identifier}

/**
 * User: chicks
 * Date: 5/30/13
 */
trait DefaultContextFactory extends ContextFactory {
  def createUndefined[T]: Context[T] = {
    new Context[T] with DefaultContextFactory {
      override val isRoot = false
      override val isUndefined = true
      val model: T = null.asInstanceOf[T]
      val parent: Context[T] = null.asInstanceOf[Context[T]]
    }
  }

  def createRoot[T](model: T): Context[T] = {
    new Context[T] with DefaultContextFactory{
      val model: T = model
      val isUndefined: Boolean = false
      val isRoot: Boolean = true
      val parent: Context[T] = createUndefined
    }
  }

  def createChild[T](model: T, parent: Context[T]): Context[T] = {
    new Context[T] with DefaultContextFactory {
      val model: T = model
      val isUndefined: Boolean = false
      val isRoot: Boolean = false
      val parent: Context[T] = parent
    }
  }
}

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
object Context {
  /**
   * mimic "falsy" values of Handlebars.js, plus care about Options
   * @param a
   * @return
   */
  def truthValue(a: Any): Boolean = a match {
    case /* UndefinedValue |*/ None | false | Nil | null | "" => false
    case _ => true
  }

  /**
   * Returns the parent of the provided context, but skips artificial levels in the hierarchy
   * introduced by Iterable, Option, etc.
   */
  def safeParent(ctx: Context[_]): Context[Any] = {
    if (ctx.isRoot || ctx.isUndefined) {
      ctx
    } else {
      ctx.parent.model match {
        case map:Map[String,_] => ctx.parent
        case list:Iterable[_] => safeParent(ctx.parent.parent)
        case _ => ctx.parent
      }
    }
  }
}

trait Context[+T] extends ContextFactory with Loggable {
  val isRoot: Boolean
  val isUndefined: Boolean
  val model: T
  val parent: Context[T]

  def asOption: Option[Context[T]] = if (isUndefined || model == null) None else Some(this)
  def notEmpty[A](fallback: Context[A]): Context[A] = if (isUndefined) fallback else this.asInstanceOf[Context[A]]

  override def toString = "Context model[%s] parent[%s]".format(model, parent)

  def lookup(path: IdentifierNode, args: List[Any] = List.empty): Context[Any] = {
    args match {
      case identifiers: List[IdentifierNode] =>
        lookup(path.value, identifiers.map(lookup(_).model))
      case _ =>
        lookup(path.value, args)
    }

  }

  def truthValue: Boolean = Context.truthValue(model)

  def lookup(path: List[String], args: List[Any]): Context[Any] = {
    path.head match {
      case p if isUndefined => this
      case ParentIdentifier(p) =>
        if (isRoot) {
          // Too many '..' in the path so return this context, or drop the '..' and
          // continue to look up the rest of the path
          if (path.tail.isEmpty) this else lookup(path.tail, args)
        } else {
          if (path.tail.isEmpty) {
            // Just the parent, '..'. Path doesn't access any property on it.
            Context.safeParent(this)
          } else {
            Context.safeParent(this).lookup(path.tail, args)
          }
        }

      case ThisIdentifier(p) => if (path.tail.isEmpty) this else lookup(path.tail, args)
      case _ =>
        model match {
          case Some(m) => createChild(m, parent).lookup(path, args)
          case map:Map[String, _] =>
            invoke(path.head, args).asOption.map {
              ctx => if (path.tail.isEmpty) ctx else ctx.lookup(path.tail, args)
            }.getOrElse(parent.lookup(path, args))
          case list:Iterable[_] => {
            if (isRoot) this else parent.lookup(path, args)
          }
          case _ => if (path.tail.isEmpty) invoke(path.head, args) else invoke(path.head, args).lookup(path.tail, args)
        }
    }
  }

  protected def invoke(methodName: String, args: List[Any] = Nil): Context[Any] = {
    getMethods(model.getClass)
      .get(methodName + args.length)
      .flatMap(invoke(_, args)).map {
        value =>
          createChild(value, this)
      }.orElse {
        model match {
          case map:Map[String, _] => map.get(methodName).map( v => createChild(v, this))
          case _ => None
        }
      }.getOrElse(createUndefined)
  }


  protected def invoke(method: Method, args: List[Any]): Option[Any] = {
    debug("Invoking method: '%s' with arguments: [%s].".format(method.getName, args.mkString(",")))

    try {
      val result = method.invoke(model, args.map(_.asInstanceOf[AnyRef]): _*)

      result match {
        case Some(o) => if (isPrimitiveType(o)) Some(o) else Some(result)
        case None => Some("")
        case _ => Some(result)
      }
    } catch {
      case e: java.lang.IllegalArgumentException => None
    }
  }



  /**
   * Returns a map containing the methods of the class - the reflection calls to generate this map
   * have been memoized so this should be performant. The method uses a read-write lock to ensure thread-safe
   * access to the map.
   *
   * @param clazz
   * @return
   */
  protected def getMethods(clazz: Class[_]): Map[String, Method] = {
     clazz.getMethods.map(m => (m.getName + m.getParameterTypes.length, m)).toMap
  }

  protected def isPrimitiveType(obj: Any) = obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
    obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]
}
