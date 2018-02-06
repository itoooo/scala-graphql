package com.github.itoooo.graphql

import com.github.itoooo.graphql.ast.Value

trait ObjectResolvable {
  def resolve(objectValue: Object, fieldName: String, argumentValues: Map[String, Value]): Object
}
