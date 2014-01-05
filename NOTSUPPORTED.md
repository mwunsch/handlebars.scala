The following are features that are supported by the original JavaScript implementation but not by the handlebars.scala
port.

- Paths with special characters. Ex.: {{foo-bar}}
- Literal Paths. Eg, {{[foo bar]}}, {{[foo bar]/expression}}, {{[@alan]/expression}}
- handlebars.scala will not throw an exception when using the 'this' keyword in paths both in normal expressions {{person/this/foo}} and helpers {{foo person/this/bar}}
- hashes using single quotes. E.g., {{foo cruel='CRUEL'}}
- paramteres as strings. {{wycats is.a slave.driver}} - The first parameter to the wycats helper will result in the empty string, not "is.a"

## Helpers

Helpers take the format of:

    (context, args, visit, inverse) => {
        // 'context' is the context of the current scope in which the helper was called

        // args is the list of params passed to a helper, e.g., {{helper "Sam"}}. args(0) -> "Sam". The param can also
        // be a reference to the context such as {{helper ../address}}

        // visit is a method that takes one parameter model:Any. model is the context that will be used when evaluating
        // the body of the helper if it is a block helper. Maps to the 'options.fn' function in JavaScript.

        // inverse is an Option of a visit method. It is the "else" case of the #if helper or the inverse of a block:
        // {{#block}}Noraml Output{{^}}Inverse Output{{/block}}. Maps to the 'options.inverse' function in JavaScript.
    }

This differs greatly from the JavaScript implementation where you have access to 'this' which would be 'context' in the
above example. The first parameter of the JavaScript helper function would be args(0) in the example above. Since we
cannot arbitrarily set the value of 'this' in Scala, handlebars.scala simply provides the raw arguments presented to the
helper to be consumed however the user wishes.