package com.gilt.handlebars.scala.logging

import scala.collection.mutable

/**
  * Created by olivierdeckers on 25/11/2016.
  */
trait AccumulatingLoggable extends Loggable {
  protected val errors: Option[mutable.MutableList[String]]

  override def warn(message: String, t: Any*): Unit = {
    errors.map(e => e += message)
    super.warn(message, t)
  }

  override def warn(message: String, t: Throwable): Unit = {
    errors.map(e => e += message)
    super.warn(message, t)
  }
}
