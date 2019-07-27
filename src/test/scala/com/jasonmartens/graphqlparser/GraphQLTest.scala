package com.jasonmartens.graphqlparser

import scala.io.Source

class GraphQLTest extends org.scalatest.FunSuite {
  test("Parsing GraphQL should succeed") {
    val jsonString = Source.fromResource("github-schema-july-26-2019.json").mkString
    IntrospectionParser.parse(jsonString)
  }

}
