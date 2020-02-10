package com.github.itoooo.graphql.parser

import com.github.itoooo.graphql.ast._
import org.parboiled2._

import scala.util.{Failure, Success, Try}

class GraphQlParser(val input: ParserInput) extends Parser {
  def document : Rule1[Document] = rule {
    zeroOrMoreWsp ~ definition.+ ~> ((definitions: Seq[Definition]) => Document(definitions))
  }

  def definition: Rule1[Definition] = rule {
    operationDefinition | fragmentDefinition
  }

  def operationDefinition: Rule1[OperationDefinition] = rule {
    selectionSet ~> { selectionSet => OperationDefinition(selectionSet, "", None, Seq[VariableDefinition](), Seq[Directive]()) } |
    operationType ~ name.? ~ zeroOrMoreWsp ~ variableDefinitions.? ~ directives.? ~ selectionSet ~> {
      (operationType: String, name: Option[String], varDef: Option[Seq[VariableDefinition]],
       directives: Option[Seq[Directive]], selectionSet: Seq[Selection]) =>
        OperationDefinition(
          selectionSet,
          operationType,
          name,
          varDef.getOrElse(Seq[VariableDefinition]()),
          directives.getOrElse(Seq[Directive]()))
    }
  }

  def operationType: Rule1[String] = rule {
    capture(atomic("query") | atomic("mutation") | atomic("subscription")) ~ zeroOrMoreWsp
  }

  def selectionSet: Rule1[Seq[Selection]] = rule {
    wsp('{') ~ selection.+ ~ wsp('}')
  }

  def selection: Rule1[Selection] = rule {
    field | fragmentSpread | inlineFragment
  }

  def field: Rule1[Field] = rule {
    alias.? ~ name ~ zeroOrMoreWsp ~ arguments.? ~ directives.? ~ selectionSet.? ~> {
      (alias: Option[String],
       name: String,
       arguments: Option[Seq[Argument]],
       directives: Option[Seq[Directive]],
       selectionSet: Option[Seq[Selection]]) =>
        Field(alias,
          name,
          arguments.getOrElse(Seq[Argument]()),
          directives.getOrElse(Seq[Directive]()),
          selectionSet.getOrElse(Seq[Selection]()))
    }
  }

  def alias: Rule1[String] = rule {
    name ~ zeroOrMoreWsp ~ wsp(':')
  }

  def arguments: Rule1[Seq[Argument]] = rule {
    wsp('(') ~ argument.+ ~ wsp(')')
  }

  def argument: Rule1[Argument] = rule {
    name ~ zeroOrMoreWsp ~ wsp(':') ~ value ~> Argument
  }

  def fragmentSpread: Rule1[FragmentSpread] = rule {
    atomic("...") ~ zeroOrMoreWsp ~ fragmentName ~ directives.? ~> ((name: String, directives: Option[Seq[Directive]]) =>
      FragmentSpread(name, directives.getOrElse(Seq[Directive]())))
  }

  def inlineFragment: Rule1[InlineFragment] = rule {
    atomic("...") ~ zeroOrMoreWsp ~ typeCondition.? ~ directives.? ~> (_.getOrElse(Seq[Directive]())) ~ selectionSet ~> InlineFragment
  }

  def fragmentDefinition: Rule1[FragmentDefinition] = rule {
    atomic("fragment") ~ zeroOrMoreWsp ~ fragmentName ~ typeCondition ~ directives.? ~> (_.getOrElse(Seq[Directive]())) ~ selectionSet ~> FragmentDefinition
  }

  def fragmentName: Rule1[String] = rule {
    name ~ zeroOrMoreWsp
  }

  def typeCondition: Rule1[String] = rule {
    atomic("on") ~ zeroOrMoreWsp ~ name ~ zeroOrMoreWsp
  }

  def value: Rule1[Value] = rule {
    variable |
    intValue |
    floatValue |
    stringValue |
    booleanValue |
    nullValue |
    enumValue |
    listValue |
    objectValue
  }

  def booleanValue: Rule1[BooleanValue] = rule {
    atomic("true") ~ zeroOrMoreWsp ~ push(BooleanValue(true)) |
      atomic("false") ~ zeroOrMoreWsp ~ push(BooleanValue(false))
  }

  def nullValue: Rule1[NullValue] = rule {
    atomic("null") ~ zeroOrMoreWsp ~ push(NullValue())
  }

  def enumValue: Rule1[EnumValue] = rule {
    name ~ zeroOrMoreWsp ~> EnumValue
  }

  def listValue: Rule1[ListValue] = rule {
    wsp('[') ~ wsp(']') ~ push(ListValue(List[Value]())) |
    wsp('[') ~ value.+ ~ wsp(']') ~> ListValue
  }

  def objectValue: Rule1[ObjectValue] = rule {
    wsp('{') ~ wsp('}') ~> (() => ObjectValue(List[ObjectField]())) |
    wsp('{') ~ objectField.+ ~ wsp('}') ~> ObjectValue
  }

  def objectField: Rule1[ObjectField] = rule {
    name ~ zeroOrMoreWsp ~ wsp(':') ~ value ~> ObjectField
  }

  def variableDefinitions: Rule1[Seq[VariableDefinition]]= rule {
    wsp('(') ~ variableDefinition.+ ~ wsp(')')
  }

  def variableDefinition: Rule1[VariableDefinition] = rule {
    variable ~ wsp(':') ~ variableType ~ defaultValue.? ~> { (variable: Variable, variableType: Type, defaultValue: Option[Value]) => VariableDefinition(variable.v, variableType, defaultValue) }
  }

  def variable: Rule1[Variable] = rule {
    wsp('$') ~ name ~ zeroOrMoreWsp ~> Variable
  }

  def defaultValue: Rule1[Value] = rule {
    wsp('=') ~ value
  }

  def variableType: Rule1[Type] = rule {
    namedType | listType | notNullType
  }

  def namedType: Rule1[NamedType] = rule {
    name ~ zeroOrMoreWsp ~> NamedType
  }

  def listType: Rule1[ListType] = rule {
    wsp('[') ~ variableType ~ wsp(']') ~> ListType
  }

  def notNullType: Rule1[NonNullType] = rule {
    namedType ~ wsp('!') ~> NonNullType |
    listType ~ wsp('!') ~> NonNullType
  }

  def directives: Rule1[Seq[Directive]] = rule {
    directive.+
  }

  def directive: Rule1[Directive] = rule {
    wsp('@') ~ name ~ zeroOrMoreWsp ~> ((name: String) => Directive(name, Seq[Argument]())) |
    wsp('@') ~ name ~ zeroOrMoreWsp ~ arguments ~> ((name: String, arguments: Seq[Argument]) => Directive(name, arguments))
  }

  def name: Rule1[String] = rule { normalName | qName }

  def normalName: Rule1[String]= rule {
    capture((CharPredicate.Alpha ++ '_') ~ oneOrMore(CharPredicate.AlphaNum ++ '_'))
  }

  def qName: Rule1[String] = rule {
    '\'' ~ capture(noneOf("'").+) ~ wsp('\'')
  }

  def stringValue: Rule1[StringValue] = rule {
    '"' ~ capture(noneOf("\"").*) ~ wsp('"') ~> StringValue
  }

  def intValue: Rule1[IntValue] = rule {
    capture('-'.? ~ CharPredicate.Digit.+) ~ zeroOrMoreWsp ~> ((v: String) => IntValue(v.toInt))
  }

  def floatValue: Rule1[FloatValue] = rule {
    capture('-'.? ~ CharPredicate.Digit.+ ~ '.' ~ CharPredicate.Digit.+) ~ zeroOrMoreWsp ~> ((s: String) => FloatValue(s.toFloat))
  }

  def zeroOrMoreWsp: Rule0 = rule {
    zeroOrMore(ch(' ') | ch('\t') | ch('\n') | ch('\r'))
  }

  def wsp(c: Char) = rule { c ~ zeroOrMoreWsp }
  def wsp(s: String) = rule { s ~ zeroOrMoreWsp }
}

class GraphQlParseError(msg: String) extends Exception(msg)

object GraphQlParser {
  def apply(query: String): Try[Document] = {
    val parser = new GraphQlParser(query)
    parser.document.run() match {
      case res @ Success(_) => res
      case Failure(err: ParseError) =>
        val message = parser.formatError(err)
        Failure(new GraphQlParseError(message))
    }
  }
}
