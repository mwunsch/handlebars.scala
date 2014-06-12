package com.gilt.handlebars.visitor

import com.gilt.handlebars.binding.{VoidBinding, BindingFactory, Binding}
import com.gilt.handlebars.logging.Loggable
import com.gilt.handlebars.parser._
import com.gilt.handlebars.helper.{HelperOptionsBuilder, Helper}
import com.gilt.handlebars.Handlebars
import com.gilt.handlebars.context.Context

object DefaultVisitor {
  def apply[T](base: Context[T], partials: Map[String, Handlebars[T]], helpers: Map[String, Helper[T]], data: Map[String, Binding[T]])(implicit bindingFactory: BindingFactory[T]) = {    
    new DefaultVisitor(base, partials, helpers, data)
  }
}

class DefaultVisitor[T](context: Context[T], partials: Map[String, Handlebars[T]], helpers: Map[String, Helper[T]], data: Map[String, Binding[T]])(implicit val contextFactory: BindingFactory[T]) extends Visitor with Loggable  {
  def visit(node: Node): String = {
    node match {
      case c:Content => visit(c)
      case c:Comment => visit(c)
      case p:Program => visit(p)
      case mustache:Mustache => visit(mustache)
      case block:Block => visit(block)
      case partial:Partial => visit(partial)
      case n => n.toString
    }
  }

  def visit(program: Program): String = program.statements.map(visit).mkString

  def visit(content: Content): String = content.value

  def visit(comment: Comment): String = ""

  def visit(mustache: Mustache): String = {
    // I. There is no hash present on this {{mustache}}

    lazy val paramsList = valueNodesToBindings(mustache.params).toList
    lazy val paramsMap = valueHashToBindingMap(mustache.hash)

    if(mustache.hash.value.isEmpty) {
      // 1. Check if path refers to a helper
      val value = helpers.get(mustache.path.string).map {
        callHelper(_, mustache, paramsList)
      }.orElse {
        // 2. Check if path exists directly in the context
        context.lookup(mustache.path, paramsList).asOption.map(_.render)
      }.orElse {
        // 3. Check if path refers to provided data.
        data.get(mustache.path.string).map(_.render)
      }.getOrElse {
        // 4. Could not find path in context, helpers or data.
        warn("Could not find path or helper: %s, context: %s".format(mustache.path, context))
        ""
      }

      escapeMustache(value, mustache.unescaped)
    } else {
      // II. There is a hash on this {{mustache}}. Start over with the hash information added to 'data'. All of the
      //     data in the hash will be accessible to any child nodes of this {{mustache}}.
      new DefaultVisitor(context, partials, helpers, data ++ paramsMap).visit(mustache.copy(hash = HashNode(Map.empty)))
    }
  }

  def visit(block: Block): String = {
    lazy val paramsList = valueNodesToBindings(block.mustache.params).toList
    lazy val paramsMap = valueHashToBindingMap(block.mustache.hash)

    // I. There is no hash present on this block
    if (block.mustache.hash.value.isEmpty) {
      val lookedUpCtx = context.lookup(block.mustache.path)
      // 1. Check if path refers to a helper
      helpers.get(block.mustache.path.string).map {
        callHelper(_, block.program, paramsList)
      }.orElse {
        // 2. Check if path exists directly in the context
        lookedUpCtx.asOption.map {
          ctx =>
            renderBlock(ctx, block.program, block.inverse)
        }
      }.getOrElse {
        // 3. path was not found in helpers or context, it will be 'falsy' by default
        renderBlock(lookedUpCtx, block.program, block.inverse)
      }
    } else {
      // II. There is a hash on this block. Start over with the hash information added to 'data'. All of the
      //     data in the hash will be accessible to any child nodes of this block.
      val blockWithoutHash = block.copy(mustache = block.mustache.copy(hash = HashNode(Map.empty)))
      new DefaultVisitor(context, partials, helpers, data ++ paramsMap).visit(blockWithoutHash)
    }
  }

  def visit(partial: Partial): String = {
    val partialName = (partial.name.value match {
      case i:IdentifierNode => i.string
      case o => o.value.toString
    }).replace("/", ".")

    val partialContext = partial.context.map(context.lookup(_)).getOrElse(context)
    partials.get(partialName).map {
      _(partialContext.binding, data, partials, helpers) // TODO - partial rendering should receive a context
    }.getOrElse {
      warn("Could not find partial: %s".format(partialName))
      ""
    }
  }

  protected def valueNodesToBindings(nodes: Iterable[ValueNode]): Iterable[Binding[T]] = {
    nodes.map {
      case p:ParameterNode =>
        contextFactory.bindPrimitiveDynamic(p.value)
      case i:IdentifierNode => {
        val value = context.lookup(i).binding.asOption getOrElse {
          Binding.mapTraverse(i.value, data)
        }
        if (! value.isDefined) warn(s"Could not lookup path ${i.value} in ${nodes}")
        value
      }
      case other =>
        contextFactory.bindPrimitiveDynamic(other)
    }
  }
  protected def valueHashToBindingMap(node: HashNode): Map[String, Binding[T]] = {
    val bindings = valueNodesToBindings(node.value.values)
    Map(node.value.keys.zip(bindings).toSeq: _*)
  }

  protected def escapeMustache(value: String, unescaped: Boolean = true): String = {
    if (unescaped) {
      value
    } else {
      scala.xml.Utility.escape(value)
    }
  }

  protected def renderBlock(ctx: Context[T], program: Program, inverse: Option[Program]): String = {
    if (ctx.truthValue) {
      ctx.map { (itemContext, idx) =>
        new DefaultVisitor(itemContext, partials, helpers, data ++ (idx.map { "index" -> contextFactory.bindPrimitive(_) })).visit(program)
      }.mkString
    } else {
      inverse.map(visit).getOrElse("")
    }
  }

  protected def callHelper(helper: Helper[T], program: Node, params: List[Binding[T]]): String = {
    val optionsBuilder = new HelperOptionsBuilder[T](context, partials, helpers, data, program, params)
    helper.apply(context.binding, optionsBuilder.build)
  }
}
