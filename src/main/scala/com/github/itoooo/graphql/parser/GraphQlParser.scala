package com.github.itoooo.graphql.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import com.github.itoooo.graphql.ast._

sealed trait Token extends Positional

case class NAME(value: String) extends Token
case class Q_NAME(value: String) extends Token
case class INT_VALUE(value: Int) extends Token
case class FLOAT_VALUE(value: Float) extends Token
case class STRING_VALUE(value: String) extends Token
case object NOT extends Token
case object DOLLAR extends Token
case object L_PAREN extends Token
case object R_PAREN extends Token
case object DOTDOTDOT extends Token
case object COLON extends Token
case object EQUALS extends Token
case object AT extends Token
case object L_SQBRACE extends Token
case object R_SQBRACE extends Token
case object L_BRACE extends Token
case object R_BRACE extends Token
case object PIPE extends Token
case object QUERY extends Token
case object MUTATION extends Token
case object SUBSCRIPTION extends Token
case object ON extends Token
case object FRAGMENT extends Token
case object TRUE extends Token
case object FALSE extends Token
case object NULL extends Token

trait CompilationError extends Exception {
  def location: Location
  def msg: String
}

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

case class LexerError(location: Location, msg: String) extends CompilationError
case class ParserError(location: Location, msg: String) extends CompilationError

object GraphQlLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\n\f]+".r

  def name: Parser[NAME] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => NAME(str) }
  }

  def qName: Parser[Q_NAME] = positioned {
    """'[^']*'""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      Q_NAME(content)
    }
  }

  def stringValue: Parser[STRING_VALUE] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      STRING_VALUE(content)
    }
  }

  def intValue: Parser[INT_VALUE] = positioned {
    "-?[0-9]+".r ^^ { str =>
      val content = str.toInt
      INT_VALUE(content)
    }
  }

  def floatValue: Parser[FLOAT_VALUE] = positioned {
    "-?[0-9]+\\.[0-9]+".r ^^ { str =>
      val content = str.toFloat
      FLOAT_VALUE(content)
    }
  }

  def not = "!" ^^ (_ => NOT)
  def dollar = "$" ^^ (_ => DOLLAR)
  def lParen = "(" ^^ (_ => L_PAREN)
  def rParen = ")" ^^ (_ => R_PAREN)
  def dotDotDot = "..." ^^ (_ => DOTDOTDOT)
  def colon = ":" ^^ (_ => COLON)
  def equals = "=" ^^ (_ => EQUALS)
  def at = "@" ^^ (_ => AT)
  def lSqbrace = "[" ^^ (_ => L_SQBRACE)
  def rSqbrace = "]" ^^ (_ => R_SQBRACE)
  def lBrace = "{" ^^ (_ => L_BRACE)
  def rBrace = "}" ^^ (_ => R_BRACE)
  def pipe = "|" ^^ (_ => PIPE)
  def query = "query" ^^ (_ => QUERY)
  def mutation = "mutation" ^^ (_ => MUTATION)
  def subscription = "subscription" ^^ (_ => SUBSCRIPTION)
  def on = "on" ^^ (_ => ON)
  def fragment = "fragment" ^^ (_ => FRAGMENT)
  def trueVal = "true" ^^ (_ => TRUE)
  def falseVal = "false" ^^ (_ => FALSE)
  def NoneVal = "None" ^^ (_ => NULL)

  def tokens: Parser[List[Token]] = {
    phrase(rep1(trueVal | falseVal | NoneVal | fragment | on | query | mutation | subscription | not | dollar |
      lParen | rParen | dotDotDot | colon | equals | at | lSqbrace | rSqbrace | lBrace | rBrace | pipe | stringValue |
      intValue | floatValue | qName | name)) ^^ {
      tokens => tokens
    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

object GraphQlParser extends Parsers {
  override type Elem = Token
  def name: Parser[String] = {
    accept("name", {
      case name @ NAME(_) => name.value
      case name @ Q_NAME(_) => name.value
    })
  }

  def document: Parser[Document] = {
    rep1(definition) ^^ (definitions => Document(definitions))
  }

  def definition: Parser[Definition] = {
    operationDefinition | fragmentDefinition
  }

  def operationDefinition: Parser[OperationDefinition] = {
    val prod1 = selectionSet ^^ {
      selectionSet => OperationDefinition(selectionSet, "query", None, Nil, Nil)
    }

    val prod2 = (operationType ~ name.? ~ variableDefinitions.? ~ directives.? ~ selectionSet) ^^ {
      case operationType ~ name ~ variableDefinitions ~ directives ~ selectionSet =>
        OperationDefinition(selectionSet, operationType, name, variableDefinitions.getOrElse(Nil), directives.getOrElse(Nil))
    }
    prod1 | prod2
  }

  def operationType: Parser[String] = {
    QUERY ^^ {_ => "query"} |
    MUTATION ^^ {_ => "mutation"} |
    SUBSCRIPTION ^^ {_ => "subscription"}
  }

  def selectionSet: Parser[List[Selection]] = {
    L_BRACE ~ rep1(selection) ~ R_BRACE ^^ {
      case _ ~ selections ~ _ => selections
    }
  }

  def selection: Parser[Selection] = {
    field | fragmentSpread | inlineFragment
  }

  def field: Parser[Field] = {
    alias.? ~ name ~ arguments.? ~ directives.? ~ selectionSet.? ^^ {
      case alias ~ name ~ arguments ~ directives ~ selectionSet =>
        Field(alias, name, arguments.getOrElse(Nil), directives.getOrElse(Nil), selectionSet.getOrElse(Nil))
    }
  }

  def alias: Parser[String] = {
    name ~ COLON ^^ {case name ~ _ => name}
  }

  def arguments: Parser[List[Argument]] = {
    L_PAREN ~ rep1(argument) ~ R_PAREN ^^ {case _ ~ arguments ~ _ => arguments}
  }

  def argument: Parser[Argument] = {
    name ~ COLON ~ value ^^ {case name ~ _ ~ value => Argument(name, value)}
  }

  def fragmentSpread: Parser[FragmentSpread] = {
    DOTDOTDOT ~ name ~ directives.? ^^ {case _ ~ name ~ directives => FragmentSpread(name, directives.getOrElse(Nil))}
  }

  def inlineFragment: Parser[InlineFragment] = {
    DOTDOTDOT ~ typeCondition.? ~ directives.? ~ selectionSet ^^ {
      case _ ~ typeCondition ~ directives ~ selectionSet => InlineFragment(typeCondition, directives.getOrElse(Nil), selectionSet)
    }
  }

  def fragmentDefinition: Parser[FragmentDefinition] = {
    FRAGMENT ~ fragmentName ~ typeCondition ~ directives.? ~ selectionSet ^^ {
      case _ ~ fragmentName ~ typeCondition ~ directives ~ selectionSet =>
        FragmentDefinition(fragmentName, typeCondition, directives.getOrElse(Nil), selectionSet)
    }
  }

  def fragmentName: Parser[String] = {
    name ^^ {name => name}
  }

  def typeCondition: Parser[String] = {
    ON ~ name ^^ {case _ ~ name => name}
  }

  def value: Parser[Value] = {
    variable | intValue | floatValue | stringValue | booleanValue | NoneValue |
    enumValue | listValue | objectValue
  }

  def intValue: Parser[IntValue] = {
    accept("IntValue", { case int @ INT_VALUE(_) => IntValue(int.value)})
  }

  def floatValue: Parser[FloatValue] = {
    accept("FloatValue", { case float @ FLOAT_VALUE(_) => FloatValue(float.value)})
  }

  def stringValue: Parser[StringValue] = {
    accept("StringValue", { case str @ STRING_VALUE(_) => StringValue(str.value)})
  }

  def booleanValue: Parser[BooleanValue] = {
    TRUE ^^ (_ => BooleanValue(true)) | FALSE ^^ (_ => BooleanValue(false))
  }

  def NoneValue: Parser[NullValue] = {
    NULL ^^ (_ => NullValue())
  }

  def enumValue: Parser[EnumValue] = {
    name ^^ (name => EnumValue(name))
  }

  def listValue: Parser[ListValue] = {
    L_SQBRACE ~ R_SQBRACE ^^ (_ => ListValue(List[Value]())) |
    L_SQBRACE ~ rep1(value) ~ R_SQBRACE ^^ {case _ ~ values ~ _ => ListValue(values)}
  }

  def objectValue: Parser[ObjectValue] = {
    L_BRACE ~ R_BRACE ^^ (_ => ObjectValue(List[ObjectField]())) |
    L_BRACE ~ rep1(objectField) ~ R_BRACE ^^ {case _ ~ fields ~ _ => ObjectValue(fields)}
  }

  def objectField: Parser[ObjectField] = {
    name ~ COLON ~ value ^^ {case name ~ _ ~ value => ObjectField(name, value)}
  }

  def variableDefinitions: Parser[List[VariableDefinition]] = {
    L_PAREN ~ rep1(variableDefinition) ~ R_PAREN ^^ {case _ ~ variableDefinitions ~ _ => variableDefinitions}
  }

  def variableDefinition: Parser[VariableDefinition] = {
    variable ~ COLON ~ variableType ~ defaultValue.? ^^ {
      case variable ~ _ ~ variableType ~ defaultValue =>
        VariableDefinition(variable.v, variableType, defaultValue)
    }
  }

  def variable: Parser[Variable] = {
    DOLLAR ~ name ^^ {case _ ~ name => Variable(name)}
  }

  def defaultValue: Parser[Value] = {
    EQUALS ~ value ^^ {case _ ~ value => value}
  }

  def variableType: Parser[Type] = {
    namedType | listType | notNullType
  }

  def namedType: Parser[NamedType] = {
    name ^^ {name => NamedType(name)}
  }

  def listType: Parser[ListType] = {
    L_SQBRACE ~ variableType ~ R_SQBRACE ^^ { case _ ~ varType ~ _ => ListType(varType) }
  }

  def notNullType: Parser[NonNullType] = {
    namedType ~ NOT ^^ {case varType ~ _ => NonNullType(varType)} |
    listType ~ NOT ^^ {case varType ~ _ => NonNullType(varType)}
  }

  def directives: Parser[List[Directive]] = {
    rep1(directive) ^^ {directives => directives}
  }

  def directive: Parser[Directive] = {
    AT ~ name ~ arguments.? ^^ {case _ ~ name ~ arguments => Directive(name, arguments.getOrElse(Nil))}
  }

  def apply(tokens: Seq[Token]): Either[ParserError, Document] = {
    val reader = new TokenReader(tokens)
    document(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}

object GraphQlCompiler {
  def apply(code: String): Either[CompilationError, Document] = {
    for {
      tokens <- GraphQlLexer(code).right
      ast <- GraphQlParser(tokens).right
    } yield ast
  }
}
