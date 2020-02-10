package com.github.itoooo.graphql

import com.github.itoooo.graphql.ast.Argument

trait GraphqlDirectiveResolver {
  val name: String
}

trait GraphQlFilterDirectiveResolver extends GraphqlDirectiveResolver {
  def resolveDirective(obj: Object, args: Seq[Argument]): Boolean
}
