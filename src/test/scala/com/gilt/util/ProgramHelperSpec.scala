package com.gilt.util

import org.specs2.mutable.Specification
import com.gilt.handlebars._
import java.io.File
import com.gilt.handlebars.Path
import com.gilt.handlebars.Partial
import com.gilt.handlebars.Program

class ProgramHelperSpec extends Specification {
  "ProgramHelperSpec" in {
    "filter out partials" in {
      val program: Program = Handlebars.fromFile(new File("src/test/resources/partialParse.handlebars")).program
      val filtered = ProgramHelper.filter(program){_.isInstanceOf[Partial]}
      filtered must beEqualTo(List(
        new Partial(new Path(List(new Identifier("localPartial")))),
        new Partial(new Path(List(new Identifier("partials"), new Identifier("aPartial")))),
        new Partial(new Path(List(new Identifier("filetest"))))
      ))
    }

    "filter out all of the content" in {
      val program: Program = Handlebars.fromFile(new File("src/test/resources/partialParse.handlebars")).program
      val filtered = ProgramHelper.filter(program){_.isInstanceOf[Content]}.map(_.asInstanceOf[Content].value.replaceAll("", "-")).mkString("")
      filtered must beEqualTo("""-
                                |-
                                |--
                                |--
                                |- - - - --
                                |- - - - --
                                |-""".stripMargin)
    }

  }
}
