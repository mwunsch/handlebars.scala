package com.gilt.handlebars.scala.partial

import java.io.File
import java.nio.charset.CodingErrorAction

import com.gilt.handlebars.scala.binding.BindingFactory
import com.gilt.handlebars.scala.parser._
import com.gilt.handlebars.scala.{Handlebars, HandlebarsImpl}

import scala.io.{Codec, Source}

/**
 * @author chicks
 * @since 6/30/13
 */
object PartialHelper extends ProgramHelper {

  /**
   * Filters a node and returns all of the Partial nodes contained within it. This method will filter as-is, so if a
   * reference to a partial exists more than once duplicates will appear in the result.
   *
   * If a unique list is desired, use toSet on the resulting List.
   *
   * @param node usually a Program node result of HandlebarsGrammar
   * @return List of Partial nodes within node
   */
  def filterPartials(node: Node): Seq[Partial] = {
    node match {
      case n:Partial => Seq(n)
      case n:Block => filterPartials(n.mustache) ++ filterPartials(n.program) ++ n.inverse.map(filterPartials(_)).getOrElse(Seq())
      case n:Mustache => filterPartials(n.path)
      case n:Program => n.statements.flatMap(filterPartials) ++ n.inverse.map(filterPartials(_)).getOrElse(Seq())
      case _ => Seq()
    }
  }

  /**
   * Finds all partials referenced in a file and nested partial includes from THOSE partials. Traverses the tree of
   * handlebars partial includes recursively and aggregates the Partials found.
   *
   * Maintains a list of files traversed to avoid infinite loops from the recursion.
   *
   * TODO: probably should not throw an error, but a warning when a template is not found. A missing template could
   *       mean that it may be defined on the frontend.
   *
   * @param file file to scan for partials
   * @param touchedFiles running list of files that were scanned
   * @return Map of partialName -> java.io.File
   */
  def findAllPartials(file: File, touchedFiles: Seq[String] = Seq())(implicit codec: Codec): Map[String, File] = {
    if (file.exists() && !touchedFiles.contains(file.getAbsolutePath)) {
      val contents = Source.fromFile(file).mkString
      val parseResult = HandlebarsGrammar(contents)
      parseResult.map { program =>
        filterPartials(program).foldLeft(Map.empty[String, File]) { (result, partial) =>
          val partialNameStr = partial.name.value.asInstanceOf[Identifier].parts.mkString("/")
          val partialFile = new File("%s/%s.handlebars".format(file.getParent, partialNameStr))

          result ++ Map(partialNameStr -> partialFile) ++ findAllPartials(partialFile, touchedFiles :+ file.getAbsolutePath)(codec)
        }
      }.getOrElse(sys.error("Could not parse template:\n\n%s".format(parseResult.toString)))
    } else {
      Map.empty
    }
  }

  def getTemplates[T](file: File)(implicit contextFactory: BindingFactory[T]): Map[String, Handlebars[T]] = {
    findAllPartials(file).map {
      case(name, partialFile) => name -> new HandlebarsImpl[T](programFromFile(partialFile), Map.empty, Map.empty)
    }
  }

  def normalizePartialNames[T](partials: Map[String, Handlebars[T]]): Map[String, Handlebars[T]] = {
    partials.map {
      case (key, value) => key.replace("/", ".") -> value
    }
  }
}
