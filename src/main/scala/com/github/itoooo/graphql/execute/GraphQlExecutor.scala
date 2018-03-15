package com.github.itoooo.graphql.execute

import com.github.itoooo.graphql._

case class GraphQlQueryError(msg: String) extends Exception

case class Variable(name: String, value: Object)

trait SchemaType

object GraphQlExecutor {
  def executeRequest(schema: Schema, document: ast.Document, operationName: Option[String],
                     variableValues: List[Variable], initialValue: ObjectResolvable): Map[String, Object] = {
    val operation = getOperation(document, operationName)
    val coercedVariablValues = coerceVariableValues(schema, operation, variableValues)
    if (operation.operationType == "query") {
      executeQuery(schema, operation, coercedVariablValues, initialValue)
    } else if (operation.operationType == "mutation") {
      executeMutation(schema, operation, coercedVariablValues, initialValue)
    } else {
      throw GraphQlQueryError("unsupported operation")
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): ast.OperationDefinition = {
    val operations = document.definitions.collect {
      case op: ast.OperationDefinition => op
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

  def coerceVariableValues(schema: Schema, operation: ast.OperationDefinition, variableValues: List[Variable]): Map[String, Object] = {
    val variableDefs = operation.variableDefinitions
    variableDefs.foldLeft(Map.empty[String, Object]) {(acc, variableDef) =>
      val value = variableValues.collectFirst {case variableValue => variableValue.name == variableDef.variable}
      if (value.isEmpty) {
        if (variableDef.defaultValue.isDefined) {
          acc + (variableDef.variable -> variableDef.defaultValue)
        } else {
          acc
        }
      } else {
        // todo: handle coercing
        acc + (variableDef.variable -> value)
      }
    }
  }

  def executeQuery(schema: Schema, query: ast.OperationDefinition, variableValues: Map[String, Object],
                   initialValue: Object): Map[String, Object] = {
    executeSelectionSet(query.selectionSet, schema.queryType, initialValue, variableValues)
  }

  def executeMutation(schema: Schema, mutation: ast.OperationDefinition, variableValues: Map[String, Object],
                      initialValue: Object) = {
    executeSelectionSet(mutation.selectionSet, schema.mutationType, initialValue, variableValues)
  }

  def executeSelectionSet(selectionSet: List[ast.Selection],
                          objectType: GraphQlObjectType,
                          objectValue: Object,
                          variableValues: Map[String, Object]): Map[String, Object] = {
    val groupedFieldSet = collectFields(selectionSet, variableValues, Nil)
    var resultMap = Map.empty[String, Object]
    groupedFieldSet.foreach({case (responseKey, fields) =>
      val fieldName = fields.head.name
      val fieldType = objectType.getFieldType(objectValue, fieldName)
      val responseValue = executeField(objectType, objectValue, fields, fieldType, variableValues)
      resultMap = resultMap + (responseKey -> responseValue)
    })
    resultMap
  }

  def collectFields(selectionSet: List[ast.Selection],
                    variableValues: Map[String, Object],
                    visitedFragments: List[ast.FragmentDefinition]): Map[String, List[ast.Field]] = {
    var groupedFields = Map.empty[String, List[ast.Field]]
    selectionSet.foreach {selection =>
      // todo: impl skip and include directive

      selection match {
        case f: ast.Field =>
          val responseKey = if (f.alias.isDefined) f.alias.get else f.name
          val groupForResponseKey = groupedFields.getOrElse(responseKey, List[ast.Field]())
          groupedFields = groupedFields + (responseKey -> (groupForResponseKey :+ f))
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
                   variableValues: Map[String, Object]): Object = {
    val field = fields.head
    val argumentValues = coerceArgumentValues(field, variableValues)
    val resolvedValue = resolveFieldValue(objectType, objectValue, field.name, argumentValues)
    completeValue(fieldType, fields, resolvedValue, variableValues)
  }

  def coerceArgumentValues(field: ast.Field,
                           variableValues: Map[String, Object]): Map[String, ast.Value] = {
    var coercedValues = Map.empty[String, ast.Value]
    val argumentValues = field.arguments
    argumentValues foreach { v =>
      coercedValues = coercedValues + (v.name -> v.value)}
    coercedValues
    /* todo: variable coerce
    val fieldName = field.name
    val argumentDefinitions = objectType.getArgDefs(fieldName)
    argumentDefinitions.foreach {argumentDefinition =>
      val argumentName = argumentDefinition.name
      val argumentType = argumentDefinition.argType
      val value = argumentValues.find(argument => argument.name == argumentName)
      if (value.isDefined) {
        val variableName = value.get.name
        val variableValue = variableValues.get(variableName)
        if (variableValue.isDefined) {
          coercedValues = coercedValues + (variableName -> variableValue.get)
        }
      }
    }
    coercedValues
    */
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
                    variableValues: Map[String, Object]): Object = {
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
}

