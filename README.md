A Scala implementation of [Handlebars](http://handlebarsjs.com/), an extension to and superset of the [Mustache](http://mustache.github.com/) templating language.

This project began as an attempt to learn Scala and to experiment with Scala's [Parser Combinators](http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.Parsers) in an attempt to get handlebars.js templates working in Scala.

Given a template:

    val template = """
      <p>Hello, my name is {{name}}. I am from {{hometown}}. I have {{kids.length}} kids:</p>
      <ul>
        {{#kids}}<li>{{name}} is {{age}}</li>{{/kids}}
      </ul>
    """

And an arbitrary Scala object:

    object Guy {
      val name = "Alan"
      val hometown = "Somewhere, TX"
      val kids = Seq(Map(
        "name" -> "Jimmy",
        "age -> "12"
      ), Map(
        "name" -> "Sally",
        "age" -> "4"
      ))
    }

Pass those into Handlebars like so:

    scala> val t = Handlebars(template)
    t: com.gilt.handlebars.Handlebars = com.gilt.handlebars.Handlebars@496d864e

    scala> t(Guy)
    res0: String =
    "
          <p>Hello, my name is Alan. I am from Somewhere, TX. I have 2 kids:</p>
          <ul>
            <li>Jimmy is 12</li><li>Sally is 4</li>
          </ul>
        "

Handlebars.scala will work just fine for [Mustache](http://mustache.github.com/mustache.5.html) templates, but includes features such as Paths and Helpers.

The example above demonstrates the `apply` method of a `Handlebars` instance, which should be familiar to Scala-fans. `apply` takes an optional second argument: a Map of helper functions. The signature for apply looks like this:

```scala
def apply[T](context: T, helpers: Map[String,Helper[T]] = Map.empty[String,Helper[T]])
```

`Handlebars.Helper[T]` translates to: `(Seq[Any], HandlebarsVisitor[T], Option[T]) => Any`

Helper functions generally look like this:

    "name" -> ((context, options, parent) => something)

where `context` is the list of arguments sent to the helper and `parent` is the surrounding context of the block. The main method of `options` you should concern yourself with is `fn`, which acts similar to Handlebars.js in that it will pass the given object into the block for evaluation. Here's an example of my go-to `head` helper:

    "head" -> ((context, option, parent) => context.head match {
      case list:Seq[_] => list.head
      case _ => context.head
    })

## Caveats

Two things to note when using Handlebars.scala:

**There is no else.** Handlebars.js handles _if/else_ type statements, but because of the nature of `else`, I did not include it here. I could not figure out a slick way to include it and stick to the functional style that Scala developers go gaga over. My Scala skills are not yet to the level where I can understand a clear path forward.

**Implicit conversions will not work in a template**. Because Handlebars.scala makes heavy use of reflection. Bummer, I know. This leads me too...

**Handlebars.scala makes heavy use of reflection**. This means that there could be unexpected behavior. Method overloading will behave in bizarre ways. There is likely a performance penalty. I'm not sophisticated enough in the arts of the JVM to know the implications of this.

## Thanks

Special thanks to the fine folks working on [Scalate](http://scalate.fusesource.org/) whose Mustache parser was my primary source of inspiration. Tom Dale and Yehuda Katz who inceptioned the idea of writing a Handlebars implementation for the JVM. The UI team at Gilt who insisted on using Handlebars and not Mustache for client-side templating. And finally, the denizens of the Scala 2.9.1 chat room at Gilt for answering my questions with enthusiastic aplomb.

## Build

The project uses [sbt](https://github.com/harrah/xsbt/wiki). Assuming you have sbt you can clone the repo, and run:

    sbt test

[![Build Status](https://secure.travis-ci.org/mwunsch/handlebars.scala.png?branch=master)](http://travis-ci.org/mwunsch/handlebars.scala)

## Copyright and License

Copyright 2013 Mark Wunsch and Gilt Groupe, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
