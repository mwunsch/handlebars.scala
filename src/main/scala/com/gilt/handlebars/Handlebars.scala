package com.gilt.handlebars

import java.io.File
import com.gilt.util.{ProgramHelper, ProfilingUtils}

sealed trait Handlebars extends ProfilingUtils {
  import Handlebars.Helper

  val program: Program

  /**
   * Applies the provided context to this template and returns the result as
   * a [[java.lang.String]]
   *
   * @param context the data to apply to the template
   * @param helpers to be used while visiting this template
   * @tparam T
   * @return the result of applying context to this template
   */
  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]): String

  def addPartial(name: String, template: Handlebars): Handlebars = {
    PartialHandlebarsCache.put(name, template)
    template
  }

  /**
   * Used to reset the handlebars to its original state. Aka, clearing the partials cache.
   */
  def reset: Unit
}

object Handlebars {
  def fromFile(file: File, delimiters: (String, String) = ("{{", "}}")): Handlebars = {
    new HandlebarsFromFile(file, delimiters)
  }

  def fromClass(clazz: Class[_], path: String): Handlebars = {
    new HandlebarsFromClass(clazz, path)
  }

  def apply(template: String, delimiters: (String, String) = ("{{", "}}")): Handlebars = {
    apply(parse(template, delimiters))
  }

  def apply(program: Program): Handlebars = {
    new HandlebarsFromProgram(program)
  }

  def parse(template: String, delimiters: (String, String) = ("{{", "}}")): Program = {
    HandlebarsGrammar(delimiters).scan(template)
  }

  def getPartials(program: Program): List[Partial] = {
    ProgramHelper.filter(program) {
      node =>
        node.isInstanceOf[Partial]
    }.map(_.asInstanceOf[Partial])
  }

  type Helper[T] = (Seq[Any], HandlebarsVisitor[T], Option[T]) => Any
}

/**
 * Creates a [[com.gilt.handlebars.Handlebars]] from a [[java.lang.Class]]. Works
 * same way as [[com.gilt.handlebars.HandlebarsFromProgram]] plus handles partials.
 *
 * @param clazz the class from which to get template as resource
 * @param path the path in the jar the class lives in to the template
 * @param delimiters the delimiters used in the template; defaults to ("{{", "}}")
 */
class HandlebarsFromClass(clazz: Class[_], path: String, delimiters: (String, String) = ("{{", "}}")) extends Handlebars {
  import Handlebars.Helper
  import com.gilt.util.SourceUtils._
  import com.gilt.handlebars.PartialHandlebarsCache._

  val jarPath = clazz.getResource(path).getPath

  val program = HandlebarsGrammar(delimiters).scan(getInputStreamContents(clazz.getResourceAsStream(path)))

  val template = new HandlebarsFromProgram(program)

  cachePartials(clazz, path, program)

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = {
    time("Rendered %s in".format(jarPath)) { template(context, helpers) }
  }

  def reset {
    PartialHandlebarsCache.clearCache
    cachePartials(clazz, path, program)
  }
}

/**
 * Creates a [[com.gilt.handlebars.Handlebars]] from a [[java.io.File]]. Works
 * same way as [[com.gilt.handlebars.HandlebarsFromProgram]] plus handles partials.
 *
 * @param file File of the handlebars template
 * @param delimiters the delimiters used in the template; defaults to ("{{", "}}")
 */
class HandlebarsFromFile(file: File, delimiters: (String, String) = ("{{", "}}")) extends Handlebars {
  import Handlebars.Helper
  import com.gilt.util.SourceUtils._
  import com.gilt.handlebars.PartialHandlebarsCache._

  require(file.exists(), "Could not find template located at [%s]".format(file.getAbsolutePath))

  val program = HandlebarsGrammar(delimiters).scan(getFileContents(file))

  val template = new HandlebarsFromProgram(program)

  cachePartials(file, program)

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = {
    time("Rendered %s in".format(file.getPath)) { template(context, helpers) }
  }

  def reset {
    PartialHandlebarsCache.clearCache
    cachePartials(file, program)
  }
}

class HandlebarsFromProgram(override val program: Program) extends Handlebars {
  import Handlebars.Helper

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = {
    HandlebarsVisitor(context, helpers).visit(program)
  }

  def reset {}
}
