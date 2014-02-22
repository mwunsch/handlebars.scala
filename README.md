A Scala implementation of [Handlebars](http://handlebarsjs.com/), an extension to and superset of the [Mustache](http://mustache.github.com/) templating language. Currently implements version [1.0.0](https://github.com/wycats/handlebars.js/tree/1.0.0) of the JavaScript version.

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

The example above demonstrates the `apply` method of a `Handlebars` instance, which should be familiar to Scala-fans. `apply` takes additional optional arguments:

+ `data` additional custom data to be referenced by @variable private variables.
+ `partials` custom partials in addition to the globally defined ones. These partials will override the globally provided ones.
+ `helpers` custom helpers in addition to the globally defined ones. These helpers will override the globally provided ones.

The signature for apply looks like this:

    def apply[T](context: T,
          data: Map[String, Any] = Map.empty,
          partials: Map[String, Handlebars] = Map.empty,
          helpers: Map[String, Helper] = Map.empty): String

## Helpers
The trait for a helper looks like this:

    trait Helper {
      def apply(model: Any, options: HelperOptions): String
    }

+ `model` the model of the context the helper was called from.
+ `options` provides helper functions to interact with the context and evaluate the body of the helper, if present.

You can define a new helper by extending the trait above, or you can use companion obejct apply method to define one on the fly:

    val fullNameHelper = Helper {
      (model, options) =>
        "%s %s".format(options.lookup("firstName"), options.lookup("lastName"))
    }

If you know that the information you need is on `model`, you can do the same thing by accessing first and last name on the model directly. However, you will be responsible for casting model to the correct type.

    val fullNameHelper = Helper {
      (model, options) =>
        val person = model.asInstanceOf[Person]
        "%s %s".format(person.firstName, person.lastName)
    }

### HelperOptions
The `HelperOption` object gives you the tools you need to get things done in your helper. The primary methods are:

+ `def argument(index: Int): Option[Any]` Retrieve an argument from the list provided to the helper by its index.
+ `def data(key: String): String` Retrieve data provided to the Handlebars template by its key.
+ `def visit(model: Any): String` Evaluates the body of the helper using the provided model as a context.
+ `def inverse(model: Any): String` Evaluate the inverse of body of the helper using the provided model as a context.
+ `def lookup(path: String): Option[Any]` Look up a path in the the current context. The one in which the helper was called.

## Caveats

**Implicit conversions will not work in a template**. Because Handlebars.scala makes heavy use of reflection. Bummer, I know. This leads me too...

**Handlebars.scala makes heavy use of reflection**. This means that there could be unexpected behavior. Method overloading will behave in bizarre ways. There is likely a performance penalty. I'm not sophisticated enough in the arts of the JVM to know the implications of this.

**Not everything from the JavaScript handlebars is supported**. See [NOTSUPPORTED](NOTSUPPORTED.md) for a list of the unsupported features. There are some things JavaScript can do that simply does not make sense to do in Scala.

## Thanks

Special thanks to the fine folks working on [Scalate](http://scalate.fusesource.org/) whose Mustache parser was my primary source of inspiration. Tom Dale and Yehuda Katz who inceptioned the idea of writing a Handlebars implementation for the JVM. The UI team at Gilt who insisted on using Handlebars and not Mustache for client-side templating. And finally, the denizens of the Scala 2.9.1 chat room at Gilt for answering my questions with enthusiastic aplomb.

## Build

The project uses [sbt](https://github.com/harrah/xsbt/wiki). Assuming you have sbt you can clone the repo, and run:

    sbt test

[![Build Status](https://secure.travis-ci.org/mwunsch/handlebars.scala.png?branch=master)](http://travis-ci.org/mwunsch/handlebars.scala)

## Copyright and License

Copyright 2014 Mark Wunsch and Gilt Groupe, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
