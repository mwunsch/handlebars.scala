package com.gilt.handlebars.scala.binding.liftjson

import com.gilt.handlebars.scala.binding.{ BindingFactory, Binding, VoidBinding }
import net.liftweb.json.{ JsonAST, Printer }
import JsonAST._

trait NumberHelpers {
  def toIntOption(number: String) = try Some(number.trim.toInt) catch { case _: Exception => None }
  def toLongOption(number: String) = try Some(number.trim.toLong) catch { case _: Exception => None }
	object IntString { def unapply(number: String) = toIntOption(number) }
}

class LiftJsonBinding(val get: JValue) extends Binding[JValue] with NumberHelpers {
  def isDefined = get match {
    case JNull | JNothing | null => false
    case _ => true
  }

  def getOrElse(default: => JValue) = if (isDefined) get else default

  def render = get match {
    case JBool(b) => b.toString
    case JInt(i) => i.toString
    case JDouble(d) => d.toString
    case JString(s) => s
    case JNull | JNothing | null => ""
    case _ => Printer.compact(JsonAST.render(get))
  }

  def isTruthy = {
    val BigZero = BigInt(0)
    get match {
      case JBool(false) | JInt(BigZero) | JDouble(0) | JString("") | JArray(Nil) | JNull | JNothing | null => false
      case _ => true
    }
  }

  def isCollection = get match {
    case JArray(_) => true
    case _ => false
  }

  def isDictionary = get match {
    case JObject(_) | JField(_, _) => true
    case _ => false
  }

  override def isPrimitive = get match {
    case JBool(_) | JInt(_) | JDouble(_) | JString(_) => true
    case _ => false
  }

  def asCollection = get match {
    case JArray(list) => list.map(LiftJsonBindingFactory.apply)
    case _ => Seq()
  }

  def asDictionaryCollection = {
    def fromJField(field: JField) = (field.name, LiftJsonBindingFactory(field.value))
    get match {
      case JObject(fields) => fields.map(fromJField)
      case field: JField => Seq(fromJField(field))
      case _ => Seq()
    }
  }

  def traverse(key: String, args: Seq[Binding[JValue]] = Seq()) = {
    def fromJField(field: JField) = LiftJsonBindingFactory(field.value)
    (get match {
      case JObject(fields) => fields.find(_.name == key).map(fromJField)
      case field: JField => Option(field).filter(_.name == key).map(fromJField)
      case JArray(list) => { key match {
        case "length" => Some(JInt(list.size))
        case IntString(i) => list.lift(i)
        case _ => None
      }}.map(LiftJsonBindingFactory.apply)
      case _ => None
    }).getOrElse(VoidBinding[JValue])
  }

  override def toString = s"LiftJsonBinding($get)"
}

object LiftJsonBindingFactory extends BindingFactory[JValue] {
  def apply(model: JValue) = new LiftJsonBinding(model)
  def bindPrimitive(s: String) = apply(JString(s))
  def bindPrimitive(i: Int) = apply(JInt(i))
  def bindPrimitive(b: Boolean) = apply(JBool(b))
  override def bindPrimitiveDynamic(v: Any) = {
    v match {
      // [begin]: it seems NOT necessary for a method called bind*Primitive* ...
      case null => apply(JNull)
      case json: JValue => apply(json)
      // [end]
      case b: Boolean => bindPrimitive(b)
      case i: Int => bindPrimitive(i)
      case f: Double => apply(JDouble(f))
      case s => bindPrimitive(s.toString)
    }
  }
}
