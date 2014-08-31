A Scala implementation of [Handlebars](http://handlebarsjs.com/), an extension to and superset of the [Mustache](http://mustache.github.com/) templating language. Currently implements version [1.0.0](https://github.com/wycats/handlebars.js/tree/1.0.0) of the JavaScript version.

This project began as an attempt to learn Scala and to experiment with Scala's [Parser Combinators](http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.Parsers) in an attempt to get handlebars.js templates working in Scala.

## Installation

If you're using SBT you can add this line to your build.sbt file.

    libraryDependencies += "com.gilt" %% "handlebars-scala" % "2.0.1"
    
## Usage

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
        "age" -> "12"
      ), Map(
        "name" -> "Sally",
        "age" -> "4"
      ))
    }

Pass those into Handlebars like so:

    scala> import com.gilt.handlebars.scala.binding.dynamic._
    import com.gilt.handlebars.scala.binding.dynamic._

    scala> import com.gilt.handlebars.scala.Handlebars
    import com.gilt.handlebars.scala.Handlebars

    scala> val t = Handlebars(template)
    t: com.gilt.handlebars.scala.Handlebars = com.gilt.handlebars.scala.Handlebars@496d864e

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
          data: Map[String, Binding[T]] = Map.empty,
          partials: Map[String, Handlebars[T]] = Map.empty,
          helpers: Map[String, Helper[T]] = Map.empty): String

## Bindings

In order to facilitate multiple ways of interacting with data, Handlebars provides a data-binding facility. Handlebars ships with a default binding strategy, DynamicBinding, which uses Scala reflection to work with scala standard-library data structures and primitives. You can implement your own Binding strategies by implementing the following traits:

- `com.gilt.handlebars.scala.binding.FullBinding`
- `com.gilt.handlebars.scala.binding.BindingFactory`

Provide the implicit BindingFactory which uses your new binding. If you need an example, see the source code in `binding/dynamic/DynamicBinding.scala`.

### Binding Interface

+ `def get: T` - Retrieve the contents of the binding. Throws runtime exception if in the void.
+ `def getOrElse(default: => T): T` - Get the contents of the binding if full; else is VoidBinding return default
+ `def toOption: Option[T]` - Similar to the Option constructor, returns Some(value) where value is defined
+ `def render: String` - Returns a string representation for the object, returning empty string where value is not defined.
+ `def traverse(key: String, args: List[Binding[T]] = List.empty): Binding[T]` For dictionaries / objects, traverse into named key, returning a binding for the matched value, VoidBinding if key is not declared.

    Important! Take note of the difference here:

    ```scala
    val binding = DynamicBinding(Map("a" -> null))
    binding.traverse("a") // => DynamicBinding(null)
    binding.traverse("b") // => VoidBinding
    ```

+ `def isTruthy: Boolean` - Returns whether the bound value evaluate to truth in handlebars if expressions?
+ `def isCollection: Boolean` - Returns whether the bound value is an iterable (and not a dictionary)
+ `def isDictionary: Boolean` - Returns whether the bound value is a dictionary
+ `def isPrimitive` - Returns whether bound value is neither collection or dictionary

+ `def asOption: Option[Binding[T]]` - If value is defined, returns Some(this), else None
+ `def asCollection: Iterable[Binding[T]]` - Returns List of bindings if isCollection; else empty List
+ `def asDictionaryCollection: Iterable[(String, Binding[T])]` - returns List of key-value tuples if isDictionary; else empty list

### `Unit` vs `null` vs `None` vs `VoidBinding`

In order to preserve the signal of "a value was defined in your model", vs., "you traversed outside the space covered by your model", bindings are monadic and capture whether they've a value from your model or not: a `FullBinding` if bound against a value from your model, a `VoidBinding` is you traversed outside the space of your model.

If the model contained a `null`, `Unit`, or `None`

`isDefined` is `true` if the bound value is within the space of the model, and it evaluates to some value other than . `VoidBinding` is, naturally, never defined, and always returns `isDefined` as `false`.

### Pattern matching value extraction

You can extract the bound value by matching `FullBinding`, like so:

```scala
DynamicBinding(1) match {
  case FullBinding(value) => value
  case VoidBinding => Unit
}
```

## Helpers

The trait for a helper looks like this:

    trait Helper[Any] {
      def apply(binding: Binding[Any], options: HelperOptions[Any]): String
    }

+ `binding` the binding for the model in the context from which the helper was called.
+ `options` provides helper functions to interact with the context and evaluate the body of the helper, if present.

You can define a new helper by extending the trait above, or you can use companion obejct apply method to define one on the fly:

    val fullNameHelper = Helper[Any] {
      (binding, options) =>
        "%s %s".format(options.lookup("firstName").renderString, options.lookup("lastName").renderString)
    }

If you know that the information you need is on `binding`, you can do the same thing by accessing first and last name on the data directly. However, you will be responsible for casting model to the correct type.

    val fullNameHelper = Helper[Any] {
      (binding, options) =>
        val person = binding.get.asInstanceOf[Person]
        "%s %s".format(person.firstName, person.lastName)
    }

### HelperOptions

The `HelperOption[T]` object gives you the tools you need to get things done in your helper. The primary methods are:

+ `def argument(index: Int): Binding[T]` Retrieve an argument from the list provided to the helper by its index.
+ `def data(key: String): Binding[T]` Retrieve data provided to the Handlebars template by its key.
+ `def visit(binding: Binding[T]): String` Evaluates the body of the helper using a context with the provided binding.
+ `def inverse(binding: Binding[T]): String` Evaluate the inverse of body of the helper using the provided model as a context.
+ `def lookup(path: String): Binding[T]` Look up a value for a path in the the current context. The one in which the helper was called.

## Caveats when using DynamicBinding

**Implicit conversions will not work in a template**. Because Handlebars.scala makes heavy use of reflection. Bummer, I know. This leads me too...

**Handlebars.scala makes heavy use of reflection**. This means that there could be unexpected behavior. Method overloading will behave in bizarre ways. There is likely a performance penalty. I'm not sophisticated enough in the arts of the JVM to know the implications of this.

**Not everything from the JavaScript handlebars is supported**. See [NOTSUPPORTED](NOTSUPPORTED.md) for a list of the unsupported features. There are some things JavaScript can do that simply does not make sense to do in Scala.

### Alternatives to DynamicBinding

If you wish for more type-safety, you can consider binding to an AST such as that provided by a popular Json library. [handlebars-play-json](https://github.com/SpinGo/handlebars-play-json) provides a binding strategy that works directly with the PlayJson AST, and provides similar truthy / collection / traversal behavior as you would find using JavaScript values in handlebars-js.

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
