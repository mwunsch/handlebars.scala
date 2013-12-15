package com.gilt.handlebars.helper

import com.gilt.handlebars.context.Context

/**
 * User: chicks
 * Date: 12/14/13
 */
trait Helper {
  def apply(context: Context[Any], args: Iterable[Any]): String
}

object Helper {
  def apply(f: ((Context[Any], Iterable[Any]) => String)): Helper = {
    new Helper {
      def apply(context: Context[Any], args: Iterable[Any]): String = f(context, args)
    }
  }
}
