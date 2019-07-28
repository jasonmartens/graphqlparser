package com.jasonmartens.graphqlparser

import org.scalatest.Matchers

import scala.io.Source

class GraphQLTest extends org.scalatest.FunSuite with Matchers {
  test("Parsing GraphQL should succeed") {
    val jsonString = Source.fromResource("github-schema-july-26-2019.json").mkString
    val result = IntrospectionParser.parseIntrospection(jsonString)
    result.length shouldBe 1
  }

}
