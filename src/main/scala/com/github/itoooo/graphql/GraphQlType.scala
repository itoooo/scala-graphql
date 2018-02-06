package com.github.itoooo.graphql

trait GraphQlType {
}

trait GraphQlObjectType extends GraphQlType with ObjectResolvable {
  def getFieldType(o: Object, fieldName: String): GraphQlType
}

class GraphQlListType(innerType: GraphQlType) extends GraphQlType {
  def getInnerType: GraphQlType = {
    innerType
  }

  def get(o: Object): Seq[Object] = {
    o match {
      case o: Seq[Object] => o
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
