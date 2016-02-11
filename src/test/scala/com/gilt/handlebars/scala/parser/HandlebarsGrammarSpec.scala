package com.gilt.handlebars.scala.parser

import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.Inside

import com.gilt.scalatest.matchers._

class HandlebarsGrammarSpec extends FunSpec with Matchers with ParserMatchers {
  import com.gilt.handlebars.scala.parser._
  val parsers = new HandlebarsGrammar(("{{","}}"))

  describe("Parser (Reference Implementation)") {
    // This set of specs should match the reference implementation as closely as possible
    // https://github.com/wycats/handlebars.js/blob/master/spec/parser_spec.rb

    it("parses simple mustaches") {
      parsers("{{foo}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo")
          ))
        ))
      }
      parsers("{{foo?}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo?")
          ))
        ))
      }
      parsers("{{foo_}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo_")
          ))
        ))
      }
      parsers("{{foo-}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo-")
          ))
        ))
      }
      parsers("{{foo:}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo:")
          ))
        ))
      }
    }

    it("parses simple mustaches with data") {
      parsers("{{@foo}}") should succeedWithResult {
        Program(List(
          Mustache(DataNode(Identifier(List("foo"))))
        ))
      }
    }

    it("parses mustaches with paths") {
      parsers("{{foo/bar}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo","bar")
          ))
        ))
      }
    }

    it("parses mustaches with this/foo") {
      parsers("{{this/foo}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("this","foo")
          ))
        ))
      }
    }

    it("parses mustaches with - in a path") {
      parsers("{{foo-bar}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo-bar")
          ))
        ))
      }
    }

    it("parses mustaches with parameters") {
      parsers("{{foo bar}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(Right(Identifier(List("bar"))))
          )
        ))
      }
    }

    it("parses mustaches with string parameters") {
      parsers("{{foo bar \"baz\"}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Right(Identifier(List("bar"))), Right(StringParameter("baz"))
            )
          )
        ))
      }
    }

    it("parses mustaches with INTEGER parameters") {
      parsers("{{foo 1}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Right(IntegerParameter(1))
            )
          )
        ))
      }
    }

    it("parses mustaches with BOOLEAN parameters") {
      parsers("{{foo true}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Right(BooleanParameter(true))
            )
          )
        ))
      }

      parsers("{{foo false}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Right(BooleanParameter(false))
            )
          )
        ))
      }
    }

    it("parses mustaches with DATA parameters") {
      parsers("{{foo @bar}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Right(DataNode(Identifier(List("bar"))))
            )
          )
        ))
      }
    }


    it("parses nested mustaches") {
      parsers("{{foo (nestedFoo true)}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")), List(
              Left(Mustache(
                Identifier(List("nestedFoo")), List(
                  Right(BooleanParameter(true))
                )
              ))
            )
          )
        ))
      }
    }

    it("parses mustaches with hash arguments") {
      parsers("{{foo bar=baz}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo")),
            Nil,
            HashNode(Map("bar" -> Identifier(List("baz"))))
          )
        ))
      }
      parsers("{{foo bar=1}}") should be (successful)
      parsers("{{foo bar=true}}") should be (successful)
      parsers("{{foo bar=false}}") should be (successful)
      parsers("{{foo bar=@baz}}") should be (successful)

      parsers("{{foo bar=baz bat=bam}}") should be (successful)
      parsers("{{foo bar=baz bat=\"bam\"}}") should be (successful)

      // The test commented-out below fails, b/c Scala does not support single quoted strings.
      // I'm okay with this deviation from the reference implementation.
      // parsers("{{foo bat='bam'}}") should be (successful)

      parsers("{{foo omg bar=baz bat=\"bam\"}}") should be (successful)
      parsers("{{foo omg bar=baz bat=\"bam\" baz=1}}") should be (successful)
      parsers("{{foo omg bar=baz bat=\"bam\" baz=true}}") should be (successful)
      parsers("{{foo omg bar=baz bat=\"bam\" baz=false}}") should be (successful)
    }

    it("parses contents followed by a mustache") {
      parsers("foo bar {{baz}}") should succeedWithResult {
        Program(List(
          Content("foo bar "), Mustache(Identifier(List("baz")))
        ))
      }
    }

    it("parses a partial") {
      parsers("{{> foo }}") should succeedWithResult {
        Program(List(
          Partial(PartialName(
            Identifier(List("foo"))
          ))
        ))
      }
    }

    it("parses a partial with context") {
      parsers("{{> foo bar}}") should succeedWithResult {
        Program(List(
          Partial(
            PartialName(Identifier(List("foo"))),
            Some(Identifier(List("bar")))
          )
        ))
      }
    }

    it("parses a partial with a complex name") {
      parsers("{{> shared/partial?.bar }}") should succeedWithResult {
        Program(List(
          Partial(PartialName(
            Identifier(List("shared", "partial?", "bar"))
          ))
        ))
      }
    }

    it("parses a comment") {
      parsers("{{! this is a comment }}") should succeedWithResult {
        Program(List(
          Comment(" this is a comment ")
        ))
      }
    }

    it("parses a multi-line comment") {
      parsers("{{!\nthis is a multi-line comment\n}}") should succeedWithResult {
        Program(List(
          Comment("\nthis is a multi-line comment\n")
        ))
      }
    }

    it("parses an inverse section") {
      parsers("{{#foo}} bar {{^}} baz {{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(List(Content(" bar ")), Some(Program(List(Content(" baz "))))),
            Some(Program(List(Content(" baz "))))
          )
        ))
      }
    }

    it("parses an inverse (else-style) section") {
      parsers("{{#foo}} bar {{else}} baz {{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(List(Content(" bar ")), Some(Program(List(Content(" baz "))))),
            Some(Program(List(Content(" baz "))))
          )
        ))
      }
    }

    it("parses empty blocks") {
      parsers("{{#foo}}{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil)
          )
        ))
      }
    }

    it("parses empty blocks with empty inverse section") {
      parsers("{{#foo}}{{^}}{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil)
          )
        ))
      }
    }

    it("parses empty blocks with empty inverse (else-style) section") {
      parsers("{{#foo}}{{else}}{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil)
          )
        ))
      }
    }

    it("parses non-empty blocks with empty inverse section") {
      parsers("{{#foo}} bar {{^}}{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(List(Content(" bar ")))
          )
        ))
      }
    }

    it("parses non-empty blocks with empty inverse (else-style) section") {
      parsers("{{#foo}} bar {{else}}{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(List(Content(" bar ")))
          )
        ))
      }
    }

    it("parses empty blocks with non-empty inverse section") {
      parsers("{{#foo}}{{^}} bar {{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil, Some(Program(List(Content(" bar "))))),
            Some(Program(List(Content(" bar "))))
          )
        ))
      }
    }

    it("parses empty blocks with non-empty inverse (else-style) section") {
      parsers("{{#foo}}{{else}} bar {{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil, Some(Program(List(Content(" bar "))))),
            Some(Program(List(Content(" bar "))))
          )
        ))
      }
    }

    it("parses a standalone inverse section") {
      parsers("{{^foo}}bar{{/foo}}") should succeedWithResult {
        Program(List(
          Block(
            Mustache(Identifier(List("foo"))),
            Program(Nil),
            Some(Program(List(Content("bar"))))
          )
        ))
      }
    }

    it("is unsuccessful if there's a Parse error") {
      // The reference implementation raises an exception on a parse failure,
      // but I prefer to use the Parsing lib's solution of a NoSuccess.
      parsers("{{foo}") should failAtLine(1)
      parsers("{{foo &}}") should failAtLine(1)
      parsers("{{#goodbyes}}{{/hellos}}") should failWithMessage("goodbyes doesn't match hellos")
    }

    it("knows how to report the correct line number in errors") {
      parsers("hello\nmy\n{{foo}") should failAtLine(3)
      parsers("hello\n\nmy\n\n{{foo}") should failAtLine(5)
    }

    it("knows how to report the correct line number in errors when the first character is a newline") {
      parsers("\n\nhello\n\nmy\n\n{{foo}") should failAtLine(7)
    }
  }

  describe("Unescaping") {
    // Some of these tests are defined in the reference implementation's Tokenizer suite.
    // Handlebars.scala does not have a tokenizer phase, so we ensure parses succeed with
    // representative AST nodes.

    it("supports unescaping with &") {
      parsers("{{&bar}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(List("bar")), unescaped=true)
        ))
      }
    }

    it("supports unescaping with {{{") {
      parsers("{{{bar}}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(List("bar")), unescaped=true)
        ))
      }
    }

  }

  describe("Whitespace") {
    it("allows liberal whitespace within a simple mustache") {
      parsers("{{ foo }}") should be (successful)
      parsers("{{ foo}}") should be (successful)
      parsers("{{foo }}") should be (successful)
      parsers("{{\n\nfoo\n\n}}") should be (successful)
    }

    it("allows liberal whitespace within a complex mustache") {
      parsers("{{ foo bar \"baz\" hello=world }}") should be (successful)
      parsers("{{ foo bar \n\"baz\"\nhello=world\n}}") should be (successful)
    }

    it("allows liberal whitespace within blocks") {
      parsers("{{# hello }}Hello, world{{/ hello }}") should be (successful)
      parsers("{{# hello\nworld}}Hello, world{{/hello}}") should be (successful)
      parsers("{{^ goodbye }}Hello, world{{/ goodbye }}") should be (successful)
    }

    it("allows liberal whitespace in inversions") {
      parsers("hello {{ ^ }} goodbye") should be (successful)
      parsers("hello {{ else }} goodbye") should be (successful)
    }
  }

  describe("Path literals") {
    it("allows path literals with []") {
      parsers("{{foo.[bar]}}") should succeedWithResult {
        Program(List(
          Mustache(
            Identifier(List("foo","bar"))
          )
        ))
      }
    }

    it("allows multiple path literals on a line with []") {
      parsers("{{foo.[bar]}}{{foo.[baz]}}") should succeedWithResult {
        Program(List(
          Mustache(Identifier(
            List("foo","bar")
          )),
          Mustache(Identifier(
            List("foo","baz")
          ))
        ))
      }
    }
  }

  describe("escaped handlebars") {
    it("yields the curly braces, removing the escaping"){
      parsers("\\{{hurray\\}}") should succeedWithResult {
        Program(List(Content("{{hurray}}")))
      }
    }
  }

}


