package com.gilt.handlebars

import java.io.File
import io.Source

sealed trait Handlebars {
  import Handlebars.Helper

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]): String
}

object Handlebars {
  def fromFile(file: File, delimiters: (String, String) = ("{{", "}}")): Handlebars = {
    new HandlebarsFromFile(file, delimiters)
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

  type Helper[T] = (Seq[Any], HandlebarsVisitor[T], Option[T]) => Any
}

/**
 * Creates a [[com.gilt.handlebars.Handlebars]] from a [[java.io.File]]. Works
 * same way as [[com.gilt.handlebars.HandlebarsFromProgram]] plus handles partials.
 *
 * Partials are preloaded and cached until they are converted to Helpers and merged
 * with the helpers provided via the apply method.
 *
 * NOTE that each partial must have a unique name since they are stored globally
 * (to match the JavaScript implementation). This constraint can be worked around
 * slightly by using subdirectories to reference partials, which this method supports
 * fully.
 *
 * @param file File of the handlebars template
 * @param delimiters the delimiters used in the template; defaults to ("{{", "}}")
 */
case class HandlebarsFromFile(file: File, delimiters: (String, String) = ("{{", "}}")) extends Handlebars {
  import Handlebars.Helper

  require(file.exists(), "Could not find template located at [%s]".format(file.getAbsolutePath))

  var partials: Map[String,String] = scanPartials(file)

  val program = HandlebarsGrammar(delimiters).scan(getContents(file))

  val template = new HandlebarsFromProgram(program)

  /**
   * Safely retrieves the contents of the provided file.
   *
   * @param sourceFile the file to open and read
   * @return the entire contents of the file
   */
  def getContents(sourceFile: File): String = {
    val source: Source = Source.fromFile(sourceFile)

    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  /**
   * Recursively searches a file for partials and returns all partials as a map
   * of partialName -> fileContents
   *
   * Partials referenced are relative to the file in which they appear. This method will
   * fail if the referenced partial is not found on the filesystem.
   *
   * @param partialFile the file to scan for partials
   * @return the raw partials by name
   */
  def scanPartials(partialFile: File): Map[String, String] = {
    val program: Program = HandlebarsGrammar(delimiters).scan(getContents(partialFile))
    val paths: List[String] = program.value
      .filter(_.isInstanceOf[Partial])
      .map(_.asInstanceOf[Partial].value.value.map(_.value).mkString("/"))

    var result: Map[String, String] = Map.empty[String, String]

    paths.foreach{ p => {
      val pFile = new File("%s/%s.handlebars".format(partialFile.getParent, p))
      require(pFile.exists(), "Could not find partial template located at [%s]".format(pFile.getAbsolutePath))


      // Don't re fetch a partial already found
      if (!result.contains(p)) {
        result += (p -> getContents(pFile))
        result = result ++ scanPartials(pFile)
      }
    }}

    result
  }

  /**
   * Converts raw partials to [[com.gilt.handlebars.Handlebars.Helper]] to
   * be used in [[com.gilt.handlebars.HandlebarsVisitor]]
   *
   * @param p a map of partials from the scanPartials method
   * @tparam T
   * @return the partials as [[com.gilt.handlebars.Handlebars.Helper]]
   */
  def partials2Helpers[T](p: Map[String, String]): Map[String, Helper[T]] = {
    p.mapValues(source => ((context: Seq[Any], options: HandlebarsVisitor[T], parent: Option[T]) => source))
  }

  /**
   * Applies the provided context to this template and returns the result as
   * a [[java.lang.String]]
   *
   * @param context the data to apply to the template
   * @param helpers to be used while visiting this template
   * @tparam T
   * @return the result of applying context to this template
   */
  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = {
    template(context, helpers ++ partials2Helpers[T](partials))
  }
}

class HandlebarsFromProgram(program: Program) extends Handlebars {
  import Handlebars.Helper

  def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]]) = {
    HandlebarsVisitor(context, helpers).visit(program)
  }
}
