package com.gilt.util

import org.specs2.mutable._
import java.io.{IOException, FileInputStream, File}
import io.Source

class SourceUtilsSpec extends Specification {
  "SourceUtils" should {
    "get contents from file" in {
      val localPartialFile = new File("src/test/resources/localPartial.handlebars")
      val contents = SourceUtils.getFileContents(localPartialFile)
      contents must beEqualTo("localPartial")
    }

    "get contents from input stream" in {
      val localPartialFile = new File("src/test/resources/localPartial.handlebars")
      val stream = new FileInputStream(localPartialFile)
      val contents = SourceUtils.getInputStreamContents(stream)
      contents must beEqualTo("localPartial")
    }

    "close the source after withClose" in {
      val localPartialFile = new File("src/test/resources/localPartial.handlebars")
      val source: Source = Source.fromFile(localPartialFile)
      SourceUtils.withClose(source)(println(_))
      source.mkString must throwAn[IOException]
    }
  }
}
