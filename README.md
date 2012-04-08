A Scala implementation of [Handlebars](http://handlebarsjs.com/), an extension to and superset of the Mustache templating language.

I'm thinking it should work something like this:

    Handlebars.compile[T](template: String): (data: T) => RenderedString

Returns a function.

Still working on it. This is mostly to experiment with Scala's [Parser Combinators](http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.Parsers) and to try to get handlebars.js templates working in Scala-land.
  
