package com.gilt.util

import java.io.{InputStream, File}
import io.Source

/**
 * User: chicks
 * Date: 2/24/13
 * Time: 10:47 AM
 */
object SourceUtils {
  /**
   * Safely retrieves the contents of the provided file.
   *
   * @param sourceFile the file to open and read
   * @return the entire contents of the file
   */
  def getFileContents(sourceFile: File): String = {
    require(sourceFile.exists(), "Could not get contents for file [%s]. File Not Found.".format(sourceFile.getPath))

    withClose(Source.fromFile(sourceFile))(_.mkString)
  }

  def getInputStreamContents(stream: InputStream): String =
    withClose(Source.fromInputStream(stream))(_.mkString)


  def withClose[T](source: Source) (fn: (Source => T)) = {
    try
      fn(source)
    finally
      source.close()
  }
}
