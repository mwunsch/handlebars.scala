package com.gilt.handlebars.partial

import com.gilt.handlebars.parser._
import java.io.File
import scala.io.Source

/**
 * User: chicks
 * Date: 6/30/13
 */
trait PartialHelper {
  def filterPartials(node: Node): List[Partial] = {
    node match {
      case n:Partial => List(n)
      case n:Block => filterPartials(n.mustache) ++ filterPartials(n.program) ++ n.inverse.map(filterPartials(_)).getOrElse(List.empty)
      case n:Mustache => filterPartials(n.path)
      case n:Program => n.statements.flatMap(filterPartials(_)) ++ n.inverse.map(filterPartials(_)).getOrElse(List.empty)
      case _ => List.empty
    }
  }

  def findPartials(file: File, touchedFiles: List[String] = List.empty): Map[String, File] = {
    if (file.exists() && !touchedFiles.contains(file.getAbsolutePath)) {
      val contents = Source.fromFile(file).mkString
      val parseResult = HandlebarsGrammar(contents)
      parseResult.map { program =>
        filterPartials(program).foldLeft(Map.empty[String, File]) { (result, partial) =>
          val partialNameStr = partial.name.value.asInstanceOf[Identifier].parts.mkString("/")
          val partialFile = new File("%s/%s.handlebars".format(file.getParent, partialNameStr))

          result ++ Map(partialNameStr -> partialFile) ++ findPartials(partialFile, touchedFiles :+ file.getAbsolutePath)
        }
      }.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
    } else {
      Map.empty
    }
  }
}
