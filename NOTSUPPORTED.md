The following are features that are supported by the original JavaScript implementation but not by the handlebars.scala
port.

- Paths with special characters. Ex.: {{foo-bar}}
- Literal Paths. Eg, {{[foo bar]}}, {{[foo bar]/expression}}, {{[@alan]/expression}}
- handlebars.scala will not throw an exception when using the 'this' keyword in paths both in normal expressions {{person/this/foo}} and helpers {{foo person/this/bar}}