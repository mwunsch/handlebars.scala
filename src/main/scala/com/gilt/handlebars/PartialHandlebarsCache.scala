package com.gilt.handlebars

import java.util.concurrent.ConcurrentHashMap
import java.io.File

/**
 * Manages caching of partials has [[com.gilt.handlebars.Handlebars]]
 *
 * NOTE that each partial must have a unique name since they are stored globally
 * (to match the JavaScript implementation). This constraint can be worked around
 * slightly by using subdirectories to reference partials, which this method supports
 * fully.
 */
object PartialHandlebarsCache {
  private [this] val cache = new ConcurrentHashMap[String, Handlebars]

  def get(path: String): Option[Handlebars] = {
    Option(cache.get(path))
  }

  def put(path: String, handlebars: Handlebars) = {
    cache.putIfAbsent(path, handlebars)
  }

  def clearCache = cache.clear()

  /**
   * Searches a file in a class (jar) for partials and caches them
   *
   * Partials referenced are relative to the file in which they appear. This method will
   * fail if the referenced partial is not found on the filesystem.
   *
   * @param clz the class from which to get template as resource
   * @param path the path inside the jar the class lives in
   * @param program the [[com.gilt.handlebars.Program]] from parsing file
   */
  def cachePartials(clz: Class[_], path: String, program: Program): Unit = {
    val paths: List[String] = Handlebars.getPartials(program).map(_.value.value.map(_.value).mkString("/"))
    paths.foreach{ p => {
      val partialPath = "%s/%s.handlebars".format(new File(path).getParent, p)
      put(p, Handlebars.fromClass(clz, partialPath))
    }}
  }

  /**
   * Searches a file for partials and caches them
   *
   * Partials referenced are relative to the file in which they appear. This method will
   * fail if the referenced partial is not found on the filesystem.
   *
   * @param file the file to scan for test.partials
   * @param program the [[com.gilt.handlebars.Program]] from parsing file
   */
  def cachePartials(file: File, program: Program): Unit = {
    val paths: List[String] = Handlebars.getPartials(program).map(_.value.value.map(_.value).mkString("/"))

    paths.foreach{ p => {
      val pFile = new File("%s/%s.handlebars".format(file.getParent, p))
      require(pFile.exists(), "Could not find partial template located at [%s]".format(pFile.getAbsolutePath))
      put(p, Handlebars.fromFile(pFile))
    }}
  }
}
