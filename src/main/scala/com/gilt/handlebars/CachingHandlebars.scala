package com.gilt.handlebars

import com.gilt.handlebars.parser.{HandlebarsGrammar, Program}
import com.gilt.handlebars.partial.PartialHelper
import java.io.File
import scala.io.Source

/**
 * User: chicks
 * Date: 6/30/13
 */
trait CachingHandlebars extends Handlebars {
  def partials: Option[Map[String, Handlebars]]

  def sourceFile: Option[String]

  def reload: Handlebars
}

case class CachingHandlebarsImpl(program: Program,
                                 partials: Option[Map[String, Handlebars]],
                                 sourceFile: Option[String]) extends CachingHandlebars {

  // TODO: Warn if we getOrElse is called. Didn't know how to re-load files.
  def reload = sourceFile.map(file => CachingHandlebars.apply(new File(file))).getOrElse(this)

  def apply[T](context: T): String = "" // Call to HandlebarsVisitor
}

object CachingHandlebars extends PartialHelper {
  def apply[T](template: String,
               partials: Option[Map[String, Handlebars]] = None,
               sourceFile: Option[String] = None): Handlebars = {

    val parseResult = HandlebarsGrammar(template)

    parseResult.map {
      program =>
        CachingHandlebarsImpl(HandlebarsGrammar(template).get, partials, sourceFile)
    }.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
  }

  def apply[T](file: File): Handlebars = {
    if (file.exists()) {
      try {
        val partials = findPartials(file).mapValues(Handlebars(_))
        apply(Source.fromFile(file).mkString, Some(partials), Some(file.getAbsolutePath))
      } catch {
        case ex:Exception => sys.error("Error while loading template\n%s".format(ex))
      }

    } else {
      sys.error("Could not load template from file: %s".format(file.getAbsolutePath))
    }
  }
}
