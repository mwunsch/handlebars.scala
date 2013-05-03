package com.gilt.handlebars

sealed class HandlebarsException(msg: String, cause: Throwable) extends Exception(msg, cause)

final case class HandlebarsRuntimeException(node: Node, cause: Throwable)
extends HandlebarsException("at position %d:%d".format(node.pos.line, node.pos.column), cause) {
  def this(node: Node) = this(node, null)
}
