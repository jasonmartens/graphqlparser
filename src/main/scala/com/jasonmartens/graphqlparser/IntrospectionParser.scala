package com.jasonmartens.graphqlparser

import com.jasonmartens.graphqlparser.GraphQL.Schema
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object IntrospectionParser {
//  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

//  implicit def valueAsJson[A](value: A)(implicit encoder: Encoder[A]): Json =
//    Encoder[A](encoder)(value)

//  implicit def optionAsJson[A](op: Option[A])(implicit encoder: Encoder[A]): Json =
//    op.map(valueAsJson(_)(encoder)).getOrElse(Null)

//  implicit def valueListAsJson[A](value: List[A])(implicit encoder: Encoder[List[A]]): Json = value match {
//    case Nil  => Json.Null
//    case list => encoder(list)
//  }

//  implicit class AnyObjectToJsonImplicit[T](obj: T) {
//    implicit def toJson(implicit e: Encoder[T]): Json = obj.asJson
//  }

  def parse(json: String): Either[Error, Schema] = {
    decode[Schema](json)
  }
}
