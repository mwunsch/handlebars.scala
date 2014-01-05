package com.gilt.handlebars

import com.gilt.handlebars.parser.{HandlebarsGrammar, Program}
import com.gilt.handlebars.partial.PartialHelper
import java.io.File
import scala.io.Source
import com.gilt.handlebars.helper.Helper

/**
 * User: chicks
 * Date: 6/30/13
 */
trait CachingHandlebars extends Handlebars {
  def sourceFile: Option[String]

  def reload: Handlebars
}

case class CachingHandlebarsImpl(program: Program,
                                 partials: Map[String, Handlebars],
                                 helpers: Map[String, Helper],
                                 sourceFile: Option[String]) extends CachingHandlebars {

  // TODO: Warn if we getOrElse is called. Didn't know how to re-load files.
  def reload = sourceFile.map(file => CachingHandlebars.apply(new File(file))).getOrElse(this)

  def apply[T](context: T, data: Map[String, Any] = Map.empty): String = "" // Call to HandlebarsVisitor
}

object CachingHandlebars extends PartialHelper {
  def apply(template: String,
               partials: Map[String, Handlebars],
               helpers: Map[String, Helper],
               sourceFile: Option[String]): Handlebars = {

    val parseResult = HandlebarsGrammar(template)

    parseResult.map {
      program =>
        CachingHandlebarsImpl(HandlebarsGrammar(template).get, partials, helpers, sourceFile)
    }.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
  }

  def apply(file: File, helpers: Map[String, Helper] = Map.empty[String, Helper]): Handlebars = {
    if (file.exists()) {
      try {
        val partials = findPartials(file).mapValues(Handlebars(_))
        apply(Source.fromFile(file).mkString, partials, helpers, Some(file.getAbsolutePath))
      } catch {
        case ex:Exception => sys.error("Error while loading template\n%s".format(ex))
      }

    } else {
      sys.error("Could not load template from file: %s".format(file.getAbsolutePath))
    }
  }
}
