package com.gilt.handlebars.scala.binding.playjson

import com.gilt.handlebars.scala.binding.{ BindingFactory, FullBinding, Binding, VoidBinding }
import com.gilt.handlebars.scala.helper.Helper
import com.gilt.handlebars.scala.logging.Loggable
import java.lang.reflect.Method
import play.api.libs.json._

class PlayJsonBinding(val data: JsValue) extends FullBinding[JsValue] with Loggable {
  override def toString = s"PlayJsonBinding(${data})"
  lazy val factory = PlayJsonBindingFactory

  lazy val render =
    if (isTruthy)
      data match {
        case JsString(s)  => s
        case JsNumber(n)  => n.toString
        case JsBoolean(b) => b.toString
        case _ => data.toString
      }
    else
      ""

  lazy val isTruthy = data match {
    case JsBoolean(t)   => t
    case JsString(s)    => s != ""
    case JsNumber(n)    => n != 0
    case JsArray(seq)   => seq.nonEmpty
    case JsNull         => false
    case _ => true
  }

  lazy val isDictionary = data.isInstanceOf[JsObject]
  lazy val isCollection = data.isInstanceOf[JsArray] && ! isDictionary

  lazy val isDefined = data match {
    case JsNull => false
    case _ => true
  }

  def traverse(key: String, args: Seq[Binding[JsValue]] = Seq()): Binding[JsValue] =
    data match {
      case m: JsObject => (m \ key) match {
        case u: JsUndefined =>
          info(s"Could not traverse key ${key} in ${m}")
          VoidBinding[JsValue]
        case v: JsDefined =>
          new PlayJsonBinding(v.value)
      }
      case _ => VoidBinding[JsValue]
    }

  protected def collectionToIterable = data match {
    case JsArray(m) =>
      m.toIterable
    case _ =>
      throw new Exception("I shouldn't be here")
  }

  protected def dictionaryToIterable = data match {
    case JsObject(m) => m.toIterable
    case _ => throw new Exception("I shouldn't be here")
  }
}

object PlayJsonBindingFactory extends BindingFactory[JsValue] {
  def apply(_model: JsValue): PlayJsonBinding =
    new PlayJsonBinding(_model)

  def bindPrimitive(v: String) = apply(JsString(v))
  def bindPrimitive(b: Boolean) = apply(JsBoolean(b))
  def bindPrimitive(model: Int) = apply(JsNumber(model))
}
