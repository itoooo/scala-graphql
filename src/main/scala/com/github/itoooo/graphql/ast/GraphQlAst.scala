package com.github.itoooo.graphql.ast

trait GraphQlAst
case class Document(definitions: Seq[Definition]) extends GraphQlAst
case class Argument(name: String, value: Value) extends GraphQlAst
case class ObjectField(name: String, value: Value) extends GraphQlAst
case class VariableDefinition(variable: String, varType: Type, defaultValue: Option[Value]) extends GraphQlAst
case class Directive(name: String, arguments: Seq[Argument]) extends GraphQlAst

trait Definition extends GraphQlAst
case class OperationDefinition(selectionSet: Seq[Selection],
                               operationType: String,
                               name: Option[String],
                               variableDefinitions: Seq[VariableDefinition],
                               directives: Seq[Directive]) extends Definition
case class FragmentDefinition(name: String,
                              typeCondition: String,
                              directives: Seq[Directive],
                              selections: Seq[Selection]) extends Definition

trait Selection extends GraphQlAst {
  val directives: Seq[Directive]
}
case class Field(alias: Option[String],
                 name: String,
                 arguments: Seq[Argument],
                 directives: Seq[Directive],
                 selectionSet: Seq[Selection]) extends Selection
case class FragmentSpread(name: String, directives: Seq[Directive]) extends Selection
case class InlineFragment(typeCondition: Option[String],
                          directives: Seq[Directive],
                          selections: Seq[Selection]) extends Selection

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
