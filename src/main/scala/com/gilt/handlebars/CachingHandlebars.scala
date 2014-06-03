package com.gilt.handlebars

import com.gilt.handlebars.parser.{ HandlebarsGrammar, Program }
import com.gilt.handlebars.partial.PartialHelper
import java.io.File
import scala.io.Source
import com.gilt.handlebars.helper.Helper
import com.gilt.handlebars.context.BindingFactory
import com.gilt.handlebars.context.Binding

/**
 * User: chicks
 * Date: 6/30/13
 */
trait CachingHandlebars[T] extends Handlebars[T] {
  def sourceFile: Option[String]

  def reload: Handlebars[T]
}

case class CachingHandlebarsImpl[T](
    program: Program,
    partials: Map[String, Handlebars[T]],
    helpers: Map[String, Helper[T]],
    sourceFile: Option[String])(implicit f: BindingFactory[T]) extends CachingHandlebars[T] {

  // TODO: Warn if we getOrElse is called. Didn't know how to re-load files.
  // TODO: Use handlebars builder to construct the new instance?
  def reload = sourceFile.map(file => CachingHandlebars.apply(new File(file))).getOrElse(this)

  def apply(
    context: Binding[T],
    data: Map[String, Binding[T]] = Map.empty[String, Binding[T]],
    partials: Map[String, Handlebars[T]] = Map.empty[String, Handlebars[T]],
    helpers: Map[String, Helper[T]] = Map.empty[String, Helper[T]])(implicit c: BindingFactory[T]): String = "" // Call to HandlebarsVisitor
}

object CachingHandlebars {
  def apply[T](
    template: String,
    partials: Map[String, Handlebars[T]],
    helpers: Map[String, Helper[T]],
    sourceFile: Option[String])(implicit f: BindingFactory[T]): Handlebars[T] = {

    val parseResult = HandlebarsGrammar(template)

    parseResult.map {
      program =>
        CachingHandlebarsImpl(HandlebarsGrammar(template).get, partials, helpers, sourceFile)
    }.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
  }

  def apply[T](
    file: File,
    helpers: Map[String, Helper[T]] = Map.empty[String, Helper[T]])(implicit contextFactory: BindingFactory[T]): Handlebars[T] = {
    if (file.exists()) {
      try {
        val partials = PartialHelper.findAllPartials(file).mapValues(Handlebars(_))
        apply(Source.fromFile(file).mkString, partials, helpers, Some(file.getAbsolutePath))
      } catch {
        case ex: Exception => sys.error("Error while loading template\n%s".format(ex))
      }

    } else {
      sys.error("Could not load template from file: %s".format(file.getAbsolutePath))
    }
  }
}
