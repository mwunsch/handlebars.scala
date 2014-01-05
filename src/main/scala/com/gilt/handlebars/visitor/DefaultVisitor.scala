package com.gilt.handlebars.visitor

import com.gilt.handlebars.context.{ClassCacheableContextFactory, Context}
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser._
import com.gilt.handlebars.parser.Content
import com.gilt.handlebars.parser.Comment
import com.gilt.handlebars.parser.Program
import com.gilt.handlebars.helper.{HelperOptions, StaticHelper, Helper}

object DefaultVisitor extends ClassCacheableContextFactory {
  def apply[T](base: T, helpers: Map[String, Helper], data: Map[String, Any]) = {
    new DefaultVisitor[T](createRoot(base), helpers, data)
  }
}

class DefaultVisitor[T](context: Context[T], helpers: Map[String, Helper], data: Map[String, Any]) extends Visitor with Loggable with ClassCacheableContextFactory {
  def visit(node: Node): String = node match {
    case Content(value) => value
    case Comment(_) => ""
    case Program(statements, inverse) => statements.map(visit).mkString
    case mustache:Mustache => {

      // I. There is no hash present on this {{mustache}}
      if(mustache.hash.value.isEmpty) {
        // 1. Check if path exists directly in the context
        val value = context.lookup(mustache.path, mustache.params).asOption.map {
          _.model.toString
        }.orElse {
        // 2. Check if path refers to a helper
          helpers.get(mustache.path.string).map(callHelper(_, context, mustache, mustache.params))
        }.orElse {
        // 3. Check if path refers to provided data.
          data.get(mustache.path.string).map {
            // 3a. Check if path resolved to an IdentifierNode, probably the result of something that looks like
            //     {{path foo=bar.baz}}. 'bar.baz' in this case gets converted to an IdentifierNode
            case i:IdentifierNode => context.lookup(i).asOption.map(_.model.toString).getOrElse("")

            // 3b. The data was something else, convert it to a string
            case other => other.toString
          }
        }.getOrElse {
        // 4. Could not find path in context, helpers or data.
          warn("Could not find path or helper: %s, context: %s".format(mustache.path, context))
          ""
        }

        escapeMustache(value, mustache.unescaped)
      } else {
      // II. There is a hash on this {{mustache}}. Start over with the hash information added to 'data'. All of the
      //     data in the hash will be accessible to any child nodes of this {{mustache}}.
        new DefaultVisitor(context, helpers, data ++ hashNode2DataMap(mustache.hash)).visit(mustache.copy(hash = HashNode(Map.empty)))
      }

    }
    case Block(mustache, program, inverse) => {
      // I. There is no hash present on this block
      if (mustache.hash.value.isEmpty) {
        val lookedUpCtx = context.lookup(mustache.path)
        // 1. Check if path exists directly in the context
        lookedUpCtx.asOption.map {
          ctx =>
            renderBlock(ctx, program, inverse)
        }.orElse {
        // 2. Check if path refers to a helper
          helpers.get(mustache.path.string).map(callHelper(_, context, program, mustache.params))
        }.getOrElse {
        // 3. path was not found in context, it will be 'falsy' by default
          renderBlock(lookedUpCtx, program, inverse)
        }
      } else {
      // II. There is a hash on this block. Start over with the hash information added to 'data'. All of the
      //     data in the hash will be accessible to any child nodes of this block.
        new DefaultVisitor(context, helpers, data ++ hashNode2DataMap(mustache.hash)).visit(program)
      }

    }
    case _ => toString
  }

  protected def hashNode2DataMap(node: HashNode): Map[String, Any] = {
    node.value.map {
      case (key, value) => value match {
        case s:StringParameter => key -> s.value
        case i:IntegerParameter => key -> i.value
        case b:BooleanParameter => key -> b.value
        case other => key -> other
      }
    }
  }

  protected def escapeMustache(value: String, unescaped: Boolean = true): String = {
    if (unescaped) {
      value
    } else {
      scala.xml.Utility.escape(value)
    }
  }

  protected def renderBlock(ctx: Context[Any], program: Program, inverse: Option[Program]): String = {
    if (ctx.truthValue) {
      ctx.model match {
        case l:Iterable[_] => l.zipWithIndex.map {
          case (item, idx) => new DefaultVisitor(createChild(item, ctx), helpers, data + ("index" -> idx)).visit(program)
        }.mkString
        case _ => visit(program)
      }
    } else {
      inverse.map(visit).getOrElse("")
    }
  }

  protected def callHelper(helper: Helper, context: Context[Any], program: Node, params: List[ValueNode]): String = {
    val args = params.map {
      case i:IdentifierNode => {
        // 1. Look in the Context
        context.lookup(i).asOption.map(_.model).orElse {
        // 2. Check the global data, but treat it as a context in case the path is 'foo.bar'
          createRoot(data).lookup(i).asOption.map(_.model)
        }.getOrElse {
        // 3. Give up, path wasn't found anywhere
          warn("Path not found for helper: %s".format(i.string))
          ""
        }
      }
      case _ => toString
    }

    // TODO: Refactor all of this. the Helper object could have an overloaded apply function that will do the below.
    //       It will keep all the helper data manipulation in a single place
    val inverse: Option[Node] = program match {
      case p:Program => p.inverse
      case b:Block => b.inverse
      case _ => None
    }

    val inverseFunc = inverse.map(node => Helper.visitFunc(context, node, helpers, data))

    helper.apply(context, HelperOptions(args, Helper.visitFunc(context, program, helpers, data), inverseFunc, data))
  }
}
