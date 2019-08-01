package com.jasonmartens.graphqlparser

import cats.data.NonEmptyList
import com.jasonmartens.graphqlparser.GraphQL.{Directive, DirectiveLocation, InputValue, TypeKinds}
import org.scalatest.Matchers

import scala.io.Source

class GraphQLTest extends org.scalatest.FunSuite with Matchers {
  test("Parsing GraphQL should succeed") {
    val jsonString = Source.fromResource("github-schema-july-26-2019.json").mkString
    val result = IntrospectionParser.parseIntrospection(jsonString)
    result match {
      case Left(err) =>
        println(err)
        fail()
      case Right(schema) =>
        schema.head.types.find(_.name.contains("Query")).map( gqlType =>
          gqlType.kind shouldBe TypeKinds.Object
        )
    }
  }
  test("Parsing a directive should succeed") {
    val directiveJson = """
        |{
        |        "name": "deprecated",
        |        "description": "Marks an element of a GraphQL schema as no longer supported.",
        |        "locations": [
        |          "FIELD_DEFINITION",
        |          "ENUM_VALUE"
        |        ],
        |        "args": [
        |          {
        |            "name": "reason",
        |            "description": "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted in [Markdown](https://daringfireball.net/projects/markdown/).",
        |            "type": {
        |              "kind": "SCALAR",
        |              "name": "String",
        |              "ofType": null
        |            },
        |            "defaultValue": "\"No longer supported\""
        |          }
        |        ]
        |      }""".stripMargin
    val control = Directive(
      name = "deprecated",
      description = Some("Marks an element of a GraphQL schema as no longer supported."),
      locations = NonEmptyList(DirectiveLocation.FieldDefinition, DirectiveLocation.EnumValue :: Nil),
      args = NonEmptyList(InputValue(
        name = "reason",
        description = Some("""Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted in [Markdown](https://daringfireball.net/projects/markdown/)."""),
        `type` = GraphQL.Types.Scalar(Some("String"), None),
        defaultValue = Some("\"No longer supported\"")
      ), Nil))
    IntrospectionParser.testParser(directiveJson) match {
      case Left(error) =>
        println(error)
        fail()
      case Right(directive) =>
        directive shouldBe control
    }
  }

}
