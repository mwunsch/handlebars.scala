package com.gilt.handlebars.scala.binding

import play.api.libs.json._
import com.gilt.handlebars.scala.helper.Helper

package object playjson {
  implicit def jsValueToPlayJsonBinding(jsonValue: JsValue) =
    new PlayJsonBinding(jsonValue)
  implicit val bindingFactory = PlayJsonBindingFactory
}
