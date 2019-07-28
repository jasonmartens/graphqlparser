package com.jasonmartens.graphqlparser

import com.jasonmartens.graphqlparser.GraphQL._
import org.json4s._
import org.json4s.native.JsonMethods._


object IntrospectionParser {
  implicit val formats = DefaultFormats

  def fieldExtractor[T](name: String, obj: List[(String, JValue)])
    (implicit formats: Formats, mf: Manifest[T]): T = {
    obj.collectFirst{ case (k, v) if k == name => v.extract[T]}
      .getOrElse(throw new RuntimeException(s"Failed to extract $name from $obj"))
  }

case object TypeSerializer extends CustomSerializer[Type](format => (
  { case JObject(obj) =>
    obj.find{ case (k, v) => k == "kind"}.map{ case (_, typeKind) =>
      val name = obj.collectFirst{ case ("name", v) => v.extract[String]}
      val description = obj.collectFirst{ case ("description", v) => v.extract[String]}
    typeKind.extract[String] match {
      case TypeKinds.Scalar.stringRep =>
        Types.Scalar(name, description)
      case TypeKinds.Object.stringRep =>
        val fields = fieldExtractor[List[Field]]("fields", obj)
        val interfaces = fieldExtractor[List[Type]]("interfaces", obj)
        Types.Object(name, description, fields, interfaces)
      case TypeKinds.Interface.stringRep =>
        val fields = fieldExtractor[List[Field]]("fields", obj)
        val possibleTypes = fieldExtractor[List[Type]]("possibleTypes", obj)
        Types.Interface(name, description, fields, possibleTypes)
      case TypeKinds.Union.stringRep =>
        val possibleTypes = fieldExtractor[List[Type]]("possibleTypes", obj)
        Types.Union(name, description, possibleTypes)
      case TypeKinds.Enum.stringRep =>
        val possibleTypes = fieldExtractor[List[Type]]("possibleTypes", obj)
        val enumValues = fieldExtractor[List[EnumValue]]("enumValues", obj)
        Types.Enum(name, description, possibleTypes, enumValues)
      case TypeKinds.InputObject.stringRep =>
        val inputFields = fieldExtractor[List[InputValue]]("inputFields", obj)
        Types.InputObject(name, description, inputFields)
    }}.getOrElse(throw new RuntimeException(s"Failed to extract Type from $obj"))
  },
  {case t: GraphQL.Type => JString("")}))

  def parseIntrospection(json: String): List[Schema] = {

    parse(json).extract[List[Schema]]
  }
}
