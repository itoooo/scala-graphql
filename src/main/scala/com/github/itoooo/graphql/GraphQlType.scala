package com.github.itoooo.graphql

import com.github.itoooo.graphql.ast.VariableDefinition

import scala.collection.mutable
import scala.collection.immutable

trait GraphQlType {
}

trait GraphQlObjectType extends GraphQlType with ObjectResolvable {
  def getFieldType(o: Object, fieldName: String): GraphQlType
  def getArgDefinitions(o: Object, fieldName: String): Seq[VariableDefinition] = Nil
}

class GraphQlListType(innerType: GraphQlType) extends GraphQlType {
  def getInnerType: GraphQlType = {
    innerType
  }

  def get(o: Object): Seq[Object] = {
    o match {
      case seq: immutable.Seq[Object] => seq
      case set: immutable.Set[Object] => set.toSeq
      case seq: mutable.Seq[Object] => seq
      case set: mutable.Set[Object] => set.toSeq
    }
  }
}

trait GraphQlScalarType extends GraphQlType {
  def get(o: Object): Object = {
    o
  }
}

trait GraphQlEnumType extends GraphQlType {
}

object GraphQlStringType extends GraphQlScalarType {
}

trait GraphQlRuntimeFieldType extends GraphQlType {
  def getFieldType(o: Object): GraphQlType
}
