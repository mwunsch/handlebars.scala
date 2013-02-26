package com.gilt.util

import com.gilt.handlebars._

object ProgramHelper {
  def filter(node: Node)(implicit filterFn: Node => Boolean): List[Node] = {
    (if (filterFn(node)) List(node) else List()) ++ (node match {
      case n:Path => n.value.flatMap(filter(_))
      case n:Partial => filter(n.value)
      case n:Mustache => filter(n.value) ++ n.parameters.flatMap(filter(_))
      case n:Section => filter(n.name) ++ filter(n.value)
      case n:Program => n.value.flatMap(filter(_)) ++ n.inverse.map(filter(_)).getOrElse(List())
      case _ => List()
    })
  }
}
