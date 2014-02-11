package com.gilt.handlebars

import Handlebars.Helper

import collection.JavaConversions._

import org.slf4j.{Logger, LoggerFactory}
import java.lang.reflect.Method
import java.util.concurrent.ConcurrentHashMap
import com.gilt.util.GuavaOptionalHelper

object HandlebarsVisitor {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  def apply[T](base: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]], hash: Map[String, Any] = Map.empty[String, Any]) = {
    new HandlebarsVisitor(new RootContext(base), helpers, hash)
  }
}

class HandlebarsVisitor[T](
  context: Context[T],
  additionalHelpers: Map[String, Helper[T]] = Map.empty[String, Helper[T]],
  val hash: Map[String, Any] = Map.empty[String, Any]
) {
  import HandlebarsVisitor._

  if (logger.isDebugEnabled) {
    logger.debug("Created a visitor with context: %s".format(context.context))
  }

  def visit(node: Node): String = try {
    node match {
      case Content(content) => content
      case Identifier(ident) => context.invoke(ident).getOrElse("").toString
      case Path(path) => resolvePath(path).definedOrEmpty.context.toString
      case Comment(_) => ""
      case Partial(partial) => compilePartial(partial)
      case Mustache(stache, params, escaped) => resolveMustache(stache, params, escape = escaped)
      case Section(stache, value, inverted) => renderSection(stache.value, stache.parameters, value, inverted)
      case Program(children, _) => children.map(visit).mkString
      case _ => toString
    }
  } catch {
    case e: HandlebarsException => throw e
    case e: Exception =>
      throw new HandlebarsRuntimeException(node, e)
  }

  /**
   * Attempt to resolve a path to a value within the current context.
   * If the path cannot be resolved, return a Context with the special value UndefinedValue,
   * so that this result is propagated during parameter passing for helpers,
   * and easy to deal with in section rendering.
   *
   * @param list the list of identifier components that constitute the path
   * @param args arguments to pass to the path's value when interpreted as a helper.
   */
  def resolvePath(list: List[Identifier], args: List[Context[Any]] = Nil): Context[Any] = {
    val resolution = list.foldLeft(None: Option[Context[Any]]) { (someContext, identifier) =>
      someContext.orElse(Some(context)).flatMap { aContext =>
        if (identifier.value == "..") {
          aContext.parent
        } else {
          aContext.context match {
            case m:Map[_,_] => m.asInstanceOf[Map[String,_]].get(identifier.value).map(new ChildContext(_, Some(aContext)))
            case _ => aContext.invoke(identifier.value, args.map(_.context)).map(new ChildContext(_, Some(aContext)))
          }
        }
      }
    }

    resolution orElse {
      if (logger.isDebugEnabled) {
        logger.debug("Could not find identifier: '%s' in context. Searching in helpers.".format(list.map(_.value).mkString(".")))
      }

      helpers.get(list.head.value) map { fn =>
        val (argsWithoutHash: List[Context[Any]], hash: List[Context[Any]]) = args.span(!_.context.isInstanceOf[Tuple2[_,_]])
        val realHash = hash.map {
          case hv: Context[Tuple2[String,_]] => hv.context
          case e => logger.error("Argument "); ("","")
        }.toMap
        if (logger.isDebugEnabled) {
          logger.debug("Found: '%s' in helpers.".format(list.head.value))
        }
        HelperResult(fn(argsWithoutHash.map(_.context), HandlebarsVisitor[T](this.context.context, this.additionalHelpers, realHash), Some(context.context)), Some(context))
      }
    } getOrElse {
      logger.warn("Unable to find value '%s' in context: '%s' or available helpers.".format(list.map(_.value).mkString("/"), context.context))
      ChildContext(UndefinedValue, Some(context))
    }
  }

  def resolveMustache(path: Path, parameters: List[Argument], escape: Boolean = true): String = {
    val args = convertArgsToContexts(parameters)
    val lookup = resolvePath(path.value, args).definedOrEmpty
    val resolution = lookup.context.toString
    if (escape)
      scala.xml.Utility.escape(resolution)
    else
      resolution
  }

  /**
   * Render a section or helper block.
   * @param path the name of the section or helper.
   * @param parameters the list of paramter names to the helper (when it is a helper).
   * @param program the block to be rendered.
   * @param inverted when rendering a section, whether or it is inverted.
   */
  def renderSection(path: Path, parameters: List[Argument], program: Program, inverted: Boolean = false): String = {
    val result: String = {
      /*
       * Since the semantics of helpers differ somewhat from those of regular sections,
       * it is useful to handle them as separate cases.
       */
      resolvePath(path.value, convertArgsToContexts(parameters)) match {
        case hr: HelperResult[_] => renderHelperBlock(hr, program)
        case c => {
          if (logger.isDebugEnabled) {
            logger.debug("Inverting the block for: %s".format(path))
          }
          renderSimpleSection(c, program, inverted)
        }
      }
    }

    if (logger.isDebugEnabled) {
      logger.debug("Evaluated block as: %s".format(result))
    }
    result
  }

  /**
   * Conditionally render a helper block, based on the truth value of the helper's output.
   * @param hr the result of evaluating the helper function.
   * @param program the block to render.
   */
  private def renderHelperBlock(hr: HelperResult[Any], program: Program): String = {
    // follow Handlebrs.js behavior for "falsy" values
    if (hr.truthValue) {
      fn(hr.context)(program)
    } else {
      // could implement Handlebars.js' else-blocks in here
      // with a change to Program and the Grammar
      // ""
      program.inverse.map(fn(context.context)(_)).getOrElse("")
    }
  }

  /**
   * Render a section whose path was resolved within the current context, i.e. it is not a helper.
   * @param context the result of evaluating the section name.
   * @param program the block to render.
   * @param inverted whether or not the section is inverted.
   */
  private def renderSimpleSection(context: Context[Any], program: Program, inverted: Boolean): String = {
    // get the final context in which to render the section
    val resolution = {
      // to invert a section, simply invert the truth value of its context
      if (inverted) {
        ChildContext(!context.truthValue, context.parent)
      } else {
        context
      }
    }

    if (resolution.truthValue) {
      // follow Mustache behavior for sections
      // e.g. don't change context for true

      // If the section is inverted, we never change contexts.
      // Due to the way inversion works the context will either be
      // true or false, so this logic should always be correct.
      resolution.context match {
        case true => visit(program)
        case c => fn(c)(program)
      }
    } else {
      program.inverse.map(fn(resolution.context)(_)).getOrElse("")
    }
  }

  def compilePartial(path: Path): String = {
    val pathAsString = path.value.map(_.value).mkString("/")
    this visit {
      PartialHandlebarsCache.get(pathAsString).getOrElse {
        logger.warn("Cache miss for path[%s]".format(path))
        val template = Handlebars(HandlebarsGrammar().scan(resolvePath(path.value).context.toString))
        PartialHandlebarsCache.put(pathAsString, template)
        template
      }.program
    }
  }

  def helpers: Map[String, (Seq[Any], HandlebarsVisitor[T], Option[T]) => Any] = {
    builtinHelpers ++ additionalHelpers
  }

  // Mimicking the options of Handlebarsjs
  def fn[A](value: A) = {
    if (logger.isDebugEnabled) {
      logger.debug("Preparing Context for new value: %s".format(value))
    }
    val block = new ChildContext(value, Some(context))
    (program: Program) => value match {
      case list:Iterable[_] => list.map(i => createVisitor(new ChildContext(i, Some(block))).visit(program)).mkString
      case array:Array[_] => array.map(i => createVisitor(new ChildContext(i, Some(block))).visit(program)).mkString
      case javaList:java.util.Collection[_] => javaList.map(i => createVisitor(new ChildContext(i, Some(block))).visit(program)).mkString
      case fun:Function1[_,_] => fun.asInstanceOf[Function1[Program,String]].apply(program).toString
      case Some(v) => createVisitor(new ChildContext(v, Some(block))).visit(program)
      case None => ""
      case _ => createVisitor(block).visit(program)
    }
  }

  private def createVisitor[A](context: Context[A]) = {
    new HandlebarsVisitor(context, additionalHelpers.asInstanceOf[Map[String,Helper[A]]])
  }

  private val builtinHelpers: Map[String, Helper[T]] = Map(
    "with" -> ((context, options, parent) => options.fn(context.head)),
    "noop" -> ((context, options, parent) => options.fn(parent)),
    "if" -> ifHelper,
    "unless" -> unlessHelper,
    "each" -> ((context, options, parent) => options.fn(context.head)),
    "this" -> ((context, options, parent) => parent.get)
  )

  private def ifHelper[T](context: Seq[Any], options: HandlebarsVisitor[T], parent: Option[T]): Any = {
    if (Context.truthValue(context.head)) {
      parent match {
        case Some(n) => options.fn(n)
        case _ => None
      }
    } else {
      None
    }
  }

  // invert the truth value of the context argument and defer to the ifHelper
  private def unlessHelper[T](context: Seq[Any], options: HandlebarsVisitor[T], parent: Option[T]): Any = {
    ifHelper(List(context.headOption.map(!Context.truthValue(_)).get), options, parent)
  }

  private def convertArgToContext(arg: Argument): Context[Any] = {
    arg match {
      case path: Path => resolvePath(path.value)
      case literal: StringLiteral => ChildContext(literal.value, Some(context))
      case literal: DoubleLiteral => ChildContext(literal.value, Some(context))
      case literal: LongLiteral => ChildContext(literal.value, Some(context))
      case keyValue: KeyValue => ChildContext((keyValue.ident.value, convertArgToContext(keyValue.lit).context), Some(context))
    }
  }

  /**
   * Map each argument in the list to its resolution as a Context.
   */
  private def convertArgsToContexts(args: List[Argument]): List[Context[Any]] = {
    args map {
      convertArgToContext(_)
    }
  }

}

object ContextClassCache {
  private [this] val cache = new ConcurrentHashMap[Class[_], Map[String, Method]]

  /**
   * Returns a map containing the methods of the class - the reflection calls to generate this map
   * have been memoized so this should be performant. The method uses a read-write lock to ensure thread-safe
   * access to the map.
   *
   * @param clazz
   * @return
   */
  def getMethods(clazz: Class[_]): Map[String, Method] = {
    val methodsOpt = cache.get(clazz)
    if (methodsOpt == null) {
      val value = clazz.getMethods.map(m => (m.getName + m.getParameterTypes.length, m)).toMap
      cache.putIfAbsent(clazz, value)
      value
    } else {
      methodsOpt
    }
  }
}

object Context {
  import ContextClassCache._

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // mimic "falsy" values of Handlebars.js, plus care about Options
  def truthValue(a: Any): Boolean = a match {
    case UndefinedValue | None | false | Nil | null | "" => false
    case _ => true
  }

}

trait Context[+T] {
  import ContextClassCache._

  import Context._

  // the wrapped context with which user code deals
  val context: T

  // Option, since the root context does
  // not have a parent, but all others do
  val parent: Option[Context[Any]]

  def invoke[A](methodName: String, args: List[A] = Nil): Option[Any] = {
    getMethod(methodName, args).flatMap(invoke(_, args))
  }

  def invoke[A](method: java.lang.reflect.Method, args: List[A]): Option[Any] = {
    if (logger.isDebugEnabled) {
      logger.debug("Invoking method: '%s' with arguments: [%s].".format(method.getName, args.mkString(",")))
    }
    try {
      if (method.getReturnType eq classOf[com.google.common.base.Optional[_]]) {
        GuavaOptionalHelper.invoke(context, method, args)
      } else {
        val result = {
          if (args.isEmpty)
            method.invoke(context)
          else
            method.invoke(context, args.map(_.asInstanceOf[AnyRef]): _*)
        }
        result match {
          case Some(o) => if (isPrimitiveType(o)) Some(o) else Some(result)
          case None => Some("")
          case _ => Some(result)
        }
      }
    } catch {
      case e: java.lang.IllegalArgumentException => logger.error("Illegal argument invoking " + method.getName + " with arguments" + args.mkString,e); None
      case e: Exception => logger.error("Eror invoking " + method.getName + " with arguments" + args.mkString, e); None
    }
  }

  def isPrimitiveType(obj: Any) = obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
    obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]

  // these are lazy vals instead of functions
  // since they don't take up too much space
  // but shouldn't really need to be recomputed
  // once used
  lazy val definedOrEmpty: Context[Any] = {
    ChildContext(
      (context: Any) match {
        case UndefinedValue => ""
        case _ => context
      },
      parent)
  }

  lazy val truthValue: Boolean = Context.truthValue(context: Any)

  def getMethod[A](name: String, args: List[A] = Nil) = getMethods(context.getClass).get(name + args.length)
}

/**
 * A context whose parent is always None
 */
case class RootContext[+T](context: T) extends Context[T] { val parent = None }

/**
 * A context whose parent should never be None
 */
case class ChildContext[+T](context: T, parent: Option[Context[Any]]) extends Context[T]

/**
 * A context that has been returned by a helper
 */
case class HelperResult[+T](context: T, parent: Option[Context[Any]]) extends Context[T]

/**
 * The value to which all undefined paths resolve.
 */
case class UndefinedValue()
