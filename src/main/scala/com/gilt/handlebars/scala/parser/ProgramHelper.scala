package com.gilt.handlebars.scala.parser

import java.io.File
import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}

class TemplateNotFoundException(message: String) extends RuntimeException(message)

trait ProgramHelper {
  def programFromString(template: String): Program = {
    val parseResult = HandlebarsGrammar(template)
    parseResult.getOrElse {
      val next = parseResult.next
      sys.error(s"""Could not parse template @ line ${next.pos.line} column ${next.pos.column}:

${next.source}

${next}
""")
    }
  }

  def programFromFile(file: File): Program = {
    if (file.exists()) {
      implicit val codec = Codec("UTF-8")
      codec.onMalformedInput(CodingErrorAction.REPLACE)
      codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
      val parseResult = HandlebarsGrammar(Source.fromFile(file).mkString)
      parseResult.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
    } else {
      throw new TemplateNotFoundException("Could not load template: %s".format(file.getAbsolutePath))
    }
  }
}
