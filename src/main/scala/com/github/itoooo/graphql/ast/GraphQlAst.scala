package com.github.itoooo.graphql.ast

trait GraphQlAst
case class Document(definitions: List[Definition]) extends GraphQlAst
case class Argument(name: String, value: Value) extends GraphQlAst
case class ObjectField(name: String, value: Value)
case class VariableDefinition(variable: String, varType: Type, defaultValue: Option[Value])
case class Directive(name: String, arguments: List[Argument])

trait Definition extends GraphQlAst
case class OperationDefinition(selectionSet: List[Selection],
                               operationType: String,
                               name: Option[String],
                               variableDefinitions: List[VariableDefinition],
                               directives: List[Directive]) extends Definition
case class FragmentDefinition(name: String,
                              typeCondition: String,
                              directives: List[Directive],
                              selections: List[Selection]) extends Definition

trait Selection extends GraphQlAst {
  val directives: List[Directive]
}
case class Field(alias: Option[String],
                 name: String,
                 arguments: List[Argument],
                 directives: List[Directive],
                 selectionSet: List[Selection]) extends Selection
case class FragmentSpread(name: String, directives: List[Directive]) extends Selection
case class InlineFragment(typeCondition: Option[String],
                          directives: List[Directive],
                          selections: List[Selection]) extends Selection

trait Value extends GraphQlAst {
  def v: Any
}
case class Variable(v: String) extends Value
case class IntValue(v: Int) extends Value
case class FloatValue(v: Float) extends Value
case class StringValue(v: String) extends Value
case class BooleanValue(v: Boolean) extends Value
case class EnumValue(v: String) extends Value
case class ListValue(v: Seq[Value]) extends Value
case class ObjectValue(v: Seq[ObjectField]) extends Value
case class NullValue(v: Unit = null) extends Value

trait Type extends GraphQlAst
case class NamedType(name: String) extends Type
case class ListType(types: Type) extends Type
case class NonNullType(t: Type) extends Type
