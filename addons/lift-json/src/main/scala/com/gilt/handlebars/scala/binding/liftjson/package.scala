package com.gilt.handlebars.scala.binding

// import play.api.libs.json._
import com.gilt.handlebars.scala.helper.Helper
import net.liftweb.json.{ JsonAST, Printer }
import JsonAST._

package object liftjson {
  implicit def jsValueToPlayJsonBinding(jsonValue: JValue) =
    LiftJsonBindingFactory(jsonValue)
  implicit val bindingFactory = LiftJsonBindingFactory
}
