The following are features that are supported by the original JavaScript implementation but not by the handlebars.scala
port.

- Paths with special characters. Ex.: {{foo-bar}}
- Literal Paths. Eg, {{[foo bar]}}, {{[foo bar]/expression}}, {{[@alan]/expression}}