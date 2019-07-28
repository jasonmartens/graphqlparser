package com.jasonmartens.graphqlparser

import com.jasonmartens.graphqlparser.GraphQL.{Directive, _}
import cats.data.NonEmptyList
import upickle.core.AbortException
import upickle.default._

object IntrospectionParser {

  implicit val typeReadWrite: ReadWriter[Type] =
    readwriter[ujson.Value].bimap[Type](
      {
        case st: Types.Scalar =>
          ujson.Obj(
            ("name", write[Option[String]](st.name)),
            ("description", write[Option[String]](st.description)),
            ("kind", write[String](st.kind.stringRep))
          )
      },
      json => {
        read[String](json.obj("kind")) match {
          case TypeKinds.Scalar.stringRep =>
            Types.Scalar(
              name = (json.obj.get("name").map(read[String])),
              description = (json.obj.get("description").map(read[String]))
            )
        }
      }
    )

  implicit val inputValueReadWrite: ReadWriter[InputValue] =
    readwriter[ujson.Value].bimap[InputValue](
      iv => {ujson.Obj(
        ("name", write[String](iv.name)),
        ("description", write[Option[String]](iv.description)),
        ("type", write[Type](iv.`type`)),
        ("defaultValue", write[Option[String]](iv.defaultValue))
      )},
      json => {
        val name = read[String](json.obj("name"))
        val descriptionField = json.obj.get("description")
        val description = descriptionField.map(s => read[String](s))
        val tpe = read[Type](json.obj("type"))
        val defaultValue = (json.obj.get("defaultValue").map(read[String]))
        InputValue(name, description, tpe, defaultValue)}
    )

  implicit def nonEmptyListReadWrite[T](implicit ev: ReadWriter[T]): ReadWriter[NonEmptyList[T]] =
    readwriter[ujson.Value].bimap[NonEmptyList[T]](
      nel => ujson.Arr.from(nel.toList.map(t => write[T](t))),
      json => {
        val list = read[List[T]](json)
        NonEmptyList(list.head, list.tail)
      }
    )

  implicit val directiveLocationReadWrite: ReadWriter[DirectiveLocation] =
    readwriter[ujson.Value].bimap[DirectiveLocation](
      dl => ujson.Str(dl.stringRep),
      json => DirectiveLocation(read[String](json))
    )

  implicit val directiveReadWrite: ReadWriter[Directive] =
    readwriter[ujson.Value].bimap[Directive](
      d => ujson.Obj(
        ("name", write[String](d.name)),
        ("description", write[Option[String]](d.description)),
        ("locations", write[NonEmptyList[DirectiveLocation]](d.locations)),
        ("args", write[NonEmptyList[InputValue]](d.args))),
      json => {
        val name = read[String](json.obj("name"))
        val descriptionField = json.obj.get("description")
        val description = descriptionField.map(s => read[String](s))
        val locationField = json.obj("locations")
        val locations = read[NonEmptyList[DirectiveLocation]](locationField)
        val argField = json.obj("args")
        val args = read[NonEmptyList[InputValue]](argField)
        Directive(name, description, locations, args)}
    )
  implicit val typeKindReadWrite: ReadWriter[TypeKind] =
    readwriter[ujson.Value].bimap[TypeKind](
      tk => tk.stringRep,
      json => TypeKinds(json.str)
    )

  def testParser(json: String): Either[String, Directive] = {
    try {
      Right(read[Directive](json))
    } catch { case AbortException(clue, index, line, col, path, cause) =>
        Left(
          s"""Failed to parse on line $line, index: $index, column: $col.
             |Clue: $clue
             |Path: $path
             |""".stripMargin)
    }
  }
//  def parseIntrospection(json: String): Either[String, List[Schema]] = {
//    try {
//      Right(read[List[Schema]](json))
//    } catch { case AbortException(clue, index, line, col, path, cause) =>
//      Left(
//        s"""Failed to parse on line $line, index: $index, column: $col.
//           |Clue: $clue
//           |Path: $path
//           |""".stripMargin)
//    }
//  }
}
