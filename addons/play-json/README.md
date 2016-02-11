handlebars-play-json
====================

Play-JSON data bindings for `handlebars.scala`. Requires `play-json` `2.4.x`.

# Installation:

```scala
libraryDependencies += "com.gilt" %% "handlebars-scala-play-json" % "2.1.1"
```

# Example:

Main.scala:

```scala
package fun

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.playjson._
import play.api.libs.json._

object Main extends App {
  val json = Json.parse("""
    {
      "lucky": [1,2,3,5,8],
      "name": { "first":  "Tim"},
      "favorites": {
        "color": "blue",
        "language": "scala"
      }
    }""")

  val template = """
My name is {{name.first}}.

These are my favorite #'s:
{{#lucky}}
- {{this}}{{/lucky}}

These are my favorite things:
{{#each favorites}}
- {{@key}}: {{this}}{{/each}}
"""
  val hb = Handlebars(template)

  println(hb(json))
}
```
