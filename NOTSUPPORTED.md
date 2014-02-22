The following are features that are supported by the original JavaScript implementation but not by the handlebars.scala
port.

- Paths with special characters. Ex.: {{foo-bar}}
- Literal Paths. Eg, {{[foo bar]}}, {{[foo bar]/expression}}, {{[@alan]/expression}}
- handlebars.scala will not throw an exception when using the 'this' keyword in paths both in normal expressions {{person/this/foo}} and helpers {{foo person/this/bar}}
- hashes using single quotes. E.g., {{foo cruel='CRUEL'}}
- paramters as strings. {{wycats is.a slave.driver}} - The first parameter to the wycats helper will result in the empty string, not "is.a"

## Helpers
Helpers in handlebars.scala take the form of:

    (model, options) => {
      // Return a string as the result of this helper or evaluating its body
      // The value of 'this' is NOT the context. Don't use it. Use model or options.lookup.
    }

Helpers differs greatly from the JavaScript implementation where you have access to `this` which would be `model` in the above example. The first parameter of the JavaScript helper function would be args(0). Since we don't want to arbitrarily set the value of `this` in Scala, handlebars.scala simply provides the raw arguments presented to the helper to be consumed however the user wishes, via `options.argument`.
