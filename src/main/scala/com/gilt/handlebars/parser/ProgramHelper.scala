package com.gilt.handlebars.parser

import java.io.File
import scala.io.Source

class TemplateNotFoundException(message: String) extends RuntimeException(message)

trait ProgramHelper {
  def programFromString(template: String): Program = {
    val parseResult = HandlebarsGrammar(template)
    parseResult.getOrElse {
      sys.error("Could not parse template:\n\n%s\n%s".format(parseResult.next.source, parseResult.next))
    }
  }

  def programFromFile(file: File): Program = {
    if (file.exists()) {
      val parseResult = HandlebarsGrammar(Source.fromFile(file).mkString)
      parseResult.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
    } else {
      throw new TemplateNotFoundException("Could not load template: %s".format(file.getAbsolutePath))
    }
  }
}
