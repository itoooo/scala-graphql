package com.github.itoooo.graphql.execute

import akka.stream.scaladsl.Source
import com.github.itoooo.graphql._
import com.github.itoooo.graphql.ast.{FragmentDefinition, OperationDefinition}

case class GraphQlQueryError(msg: String) extends Exception

case class Variable(name: String, value: Object)

trait SchemaType

object GraphQlExecutor {
  type ExecuteResult = Map[String, Object]

  // for refer in fragment
  var currentDocument: ast.Document = _
  var execSchema: Schema = _

  def executeRequest(schema: Schema, document: ast.Document, operationName: Option[String],
                     variableValues: Map[String, ast.Value], initialValue: ObjectResolvable):
                     Either[ExecuteResult, Source[ExecuteResult, _]] = {
    currentDocument = document
    execSchema = schema
    val operation = getOperation(document, operationName)
    val coercedVariablValues = coerceVariableValues(schema, operation, variableValues)
    if (operation.operationType == "query") {
      Left(executeQuery(schema, operation, coercedVariablValues, initialValue))
    } else if (operation.operationType == "mutation") {
      Left(executeMutation(schema, operation, coercedVariablValues, initialValue))
    } else if (operation.operationType == "subscription") {
      Right(subscribe(schema, operation, coercedVariablValues, initialValue))
    } else {
      throw GraphQlQueryError("unsupported operation")
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): ast.OperationDefinition = {
    val operations = document.definitions flatMap {
      case e: OperationDefinition => Some(e)
      case _ => None
    }

    if (operationName.isEmpty) {
      if (operations.length == 1) {
        operations.head
      } else {
        throw GraphQlQueryError("require operation name")
      }
    } else {
      val operation = operations.collectFirst {
        case op @ ast.OperationDefinition(_, _, operationName, _, _) => op
      }
      if (operation.isDefined) {
        operation.get
      } else {
        throw GraphQlQueryError(s"'${operationName.get}' not found")
      }
    }
  }

  def coerceVariableValues(schema: Schema, operation: ast.OperationDefinition,
                           variableValues: Map[String, ast.Value]): Map[String, ast.Value] = {
    // todo: handle coercing
    variableValues
  }

  def executeQuery(schema: Schema, query: ast.OperationDefinition, variableValues: Map[String, ast.Value],
                   initialValue: Object): Map[String, Object] = {
    executeSelectionSet(query.selectionSet, schema.queryType, initialValue, variableValues)
  }

  def executeMutation(schema: Schema, mutation: ast.OperationDefinition, variableValues: Map[String, ast.Value],
                      initialValue: Object): Map[String, Object] = {
    executeSelectionSet(mutation.selectionSet, schema.mutationType, initialValue, variableValues)
  }

  def subscribe(schema: Schema, subscription: ast.OperationDefinition, variableValues: Map[String, ast.Value],
                initialValue: Object): Source[Map[String, Object], _] = {
    createSourceEventStream(subscription, schema.subscriptionType, variableValues, initialValue)
  }

  def executeSelectionSet(selectionSet: List[ast.Selection],
                          objectType: GraphQlObjectType,
                          objectValue: Object,
                          variableValues: Map[String, ast.Value]): Map[String, Object] = {
    val groupedFieldSet = collectFields(objectType, objectValue, selectionSet, variableValues)
    var resultMap = Map.empty[String, Object]
    groupedFieldSet.foreach({case (responseKey, fields) =>
      val fieldName = fields.head.name
      val fieldType = objectType.getFieldType(objectValue, fieldName)
      val responseValue = executeField(objectType, objectValue, fields, fieldType, variableValues)
      resultMap = resultMap + (responseKey -> responseValue)
    })
    resultMap
  }

  def handleDirective(directive: ast.Directive, objectValue: Object): Boolean = {
    val directiveDefs = execSchema.directiveDefs
    val filterDirectiveDefs = directiveDefs.collect { case x: GraphQlFilterDirectiveResolver => x }
    filterDirectiveDefs.find(_.name == directive.name) match {
      case Some(directiveResolver: GraphQlFilterDirectiveResolver) =>
        directiveResolver.resolveDirective(objectValue, directive.arguments)
      case None => throw GraphQlQueryError(s"undefined directive ${directive.name}")
    }
  }


  def collectFields(objectType: GraphQlObjectType,
                    objectValue: Object,
                    selectionSet: List[ast.Selection],
                    variableValues: Map[String, Object],
                    visitedFragments: List[String] = List()): Map[String, List[ast.Field]] = {
    var groupedFields = Map.empty[String, List[ast.Field]]
    selectionSet.foreach {selection =>
      // todo: impl skip and include directive

      selection match {
        case f: ast.Field =>
          // if directives has specified, evaluate all directives and
          // include a field when all result are 'true'.
          var include = true
          if (f.directives.nonEmpty) {
            include = f.directives.forall(handleDirective(_, objectValue))
          }
          if (include) {
            val responseKey = if (f.alias.isDefined) f.alias.get else f.name
            val groupForResponseKey = groupedFields.getOrElse(responseKey, List[ast.Field]())
            groupedFields = groupedFields + (responseKey -> (groupForResponseKey :+ f))
          }
        case f: ast.FragmentSpread =>
          if (!visitedFragments.contains(f.name)) {
            // find Fragment in the current document
            val fragment = currentDocument.definitions.flatMap {
              case e: FragmentDefinition => Some(e)
              case _ => None
            }.find(e => e.name == f.name)

            if (fragment.isDefined) {
              // todo: impl DoesFragmentTypeApply

              val fragmentSelectionSet = fragment.get.selections
              val fragmentGroupedFieldSet = collectFields(objectType, objectValue,
                fragmentSelectionSet, variableValues, visitedFragments :+ f.name)
              for ((responseKey, fragmentGroup) <- fragmentGroupedFieldSet) {
                var groupForResponseKey = groupedFields.getOrElse(responseKey, List[ast.Field]())
                fragmentGroup foreach { e =>
                  groupForResponseKey = groupForResponseKey :+ e
                }
                groupedFields = groupedFields + (responseKey -> groupForResponseKey)
              }
            }
          }
        case _: Any =>
      }

      None
    }
    groupedFields
  }

  def executeField(objectType: GraphQlObjectType,
                   objectValue: Object,
                   fields: List[ast.Field],
                   fieldType: GraphQlType,
                   variableValues: Map[String, ast.Value]): Object = {
    val field = fields.head
    val argumentValues = coerceArgumentValues(field, variableValues)
    val resolvedValue = resolveFieldValue(objectType, objectValue, field.name, argumentValues)
    completeValue(fieldType, fields, resolvedValue, variableValues)
  }

  def coerceArgumentValues(field: ast.Field,
                           variableValues: Map[String, ast.Value]): Map[String, ast.Value] = {
    var coercedValues = Map.empty[String, ast.Value]
    val argumentValues = field.arguments
    argumentValues foreach { argument =>
      argument.value match {
        case variable: ast.Variable =>
          val variableValue = variableValues.get(variable.v)
          if (variableValue.isDefined) {
            coercedValues = coercedValues + (argument.name -> variableValue.get)
          } else {
            throw GraphQlQueryError(s"variable '${variable.v}' not found")
          }
        case _ =>
          coercedValues = coercedValues + (argument.name -> argument.value)
      }
    }
    coercedValues
  }

  def resolveFieldValue(objectType: ObjectResolvable,
                        objectValue: Object,
                        fieldName: String,
                        argumentValues: Map[String, ast.Value]): Object = {
    objectType.resolve(objectValue, fieldName, argumentValues)
  }

  def completeValue(fieldType: GraphQlType,
                    fields: List[ast.Field],
                    result: Object,
                    variableValues: Map[String, ast.Value]): Object = {
    // todo: handle non-null type

    if (result == null || result == None) {
      None
    } else {
      fieldType match {
        case listType: GraphQlListType =>
          val list = listType.get(result)
          val innerType = listType.getInnerType
          list.map { e => completeValue(innerType, fields, e, variableValues) }
        case scalar: GraphQlScalarType =>
          scalar.get(result)
        case _: GraphQlEnumType =>
          result
        case objectType: GraphQlObjectType =>
          val subSelectionSet = mergeSelectionSets(fields)
          executeSelectionSet(subSelectionSet, objectType, result, variableValues)
        case runtimeFieldType: GraphQlRuntimeFieldType =>
          val runtimeType = runtimeFieldType.getFieldType(result)
          completeValue(runtimeType, fields, result, variableValues)
      }
    }
  }

  def mergeSelectionSets(fields: List[ast.Field]): List[ast.Selection] = {
    fields.foldLeft(List.empty[ast.Selection]) { (acc, field) =>
      acc ::: field.selectionSet
    }
  }

  def createSourceEventStream(subscription: OperationDefinition, subscriptionType: GraphQlObjectType,
                              variableValues: Map[String, ast.Value], initialValue: Object): Source[Map[String, Object], _] = {
    val groupedFieldSet = collectFields(subscriptionType, initialValue, subscription.selectionSet, variableValues)
    val fields = groupedFieldSet.head
    val fieldName = fields._1
    val field = fields._2.head
    val argumentValues = coerceArgumentValues(field, variableValues)
    val sourceStream = resolveFieldEventStream(subscriptionType, initialValue, fieldName, argumentValues)

    // MapSourceToResponseEvent
    val fieldType = subscriptionType.getFieldType(initialValue, fieldName)
    sourceStream.map { ev =>
      // ExecuteSubscriptionEvent
      subscription.selectionSet.head match {
        case f: ast.Field =>
          val res = executeSelectionSet(f.selectionSet, fieldType.asInstanceOf[GraphQlObjectType],
            ev, variableValues)
          res
        case selectionSet => throw GraphQlQueryError(s"not contained selection set in ${selectionSet}")
      }
    }
  }

  def resolveFieldEventStream(objectType: ObjectResolvable, rootValue: Object, fieldName: String,
                              argumentValues: Map[String, ast.Value]): Source[Object, _] = {
    objectType.resolve(rootValue, fieldName, argumentValues).asInstanceOf[Source[Object, _]]
  }
}
