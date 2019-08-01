package com.jasonmartens.graphqlparser

import com.jasonmartens.graphqlparser.GraphQL.{Directive, _}
import cats.data.NonEmptyList
import upickle.core.AbortException
import upickle.default._
import java.util.NoSuchElementException

object IntrospectionParser {

  def readListT[T](json: ujson.Value, fieldName: String)(implicit ev: Reader[T]): List[T] = {
    json.obj.get("field") match {
      case Some(f) => read[List[T]](f)
      case None => List.empty[T]
    }
  }

  def readOptionT[T](json: ujson.Value, fieldName: String)(implicit ev: Reader[T]): Option[T] = {
      json.obj.get(fieldName) match {
        case Some(f) if f.isNull => None
        case Some(f) => Some(read[T](f))
        case None => None
      }
  }

  def readT[T](json: ujson.Value, fieldName: String)(implicit ev: Reader[T]): T = {
    try {
      read[T](json.obj(fieldName))
    } catch {
      case ex: NoSuchElementException =>
        println(s"Failed to extract $fieldName: $ex. json:\n$json")
        throw ex
    }
  }

  implicit val schemaReadWrite: ReadWriter[Schema] =
    readwriter[ujson.Value].bimap[Schema](
      schema => {
        ujson.Obj(
          ("types", write[NonEmptyList[Type]](schema.types)),
          ("queryType", write[QueryType](schema.queryType)),
          ("mutationType", write[Option[MutationType]](schema.mutationType)),
          ("subscriptionType", write[Option[SubscriptionType]](schema.subscriptionType)),
          ("directives", write[NonEmptyList[Directive]](schema.directives))
        )
      },
      json => {
        val types = readT[NonEmptyList[Type]](json, "types")
        val queryType = readT[QueryType](json, "queryType")
        val mutationType = readOptionT[MutationType](json, "mutationType")
        val subscriptionType = readOptionT[SubscriptionType](json, "subscriptionType")
        val directives = readT[NonEmptyList[Directive]](json, "directives")
        Schema(types, queryType, mutationType, subscriptionType, directives)
      }
    )

  implicit val queryTypeReadWrite: ReadWriter[QueryType] = {
    readwriter[ujson.Value].bimap[QueryType](
      t => ujson.Obj(("name", write[String](t.name))),
      json => QueryType(readT[String](json, "name"))
    )
  }

  implicit val mutationTypeReadWrite: ReadWriter[MutationType] = {
    readwriter[ujson.Value].bimap[MutationType](
      t => ujson.Obj(("name", write[String](t.name))),
      json => MutationType(readT[String](json, "name"))
    )
  }

  implicit val subscriptionTypeReadWrite: ReadWriter[SubscriptionType] = {
    readwriter[ujson.Value].bimap[SubscriptionType](
      t => ujson.Obj(("name", write[String](t.name))),
      json => SubscriptionType(readT[String](json, "name"))
    )
  }

  implicit val typeReadWrite: ReadWriter[Type] =
    readwriter[ujson.Value].bimap[Type](
      {
        case st: Types.Scalar =>
          ujson.Obj(
            ("name", write[Option[String]](st.name)),
            ("description", write[Option[String]](st.description)),
            ("kind", write[String](st.kind.stringRep))
          )
        case _ => ??? // TODO: Other types
      },
      json => {
        val name = readOptionT[String](json, "name")
        val description = readOptionT[String](json, "description")
        readT[String](json, "kind") match {
          case TypeKinds.Scalar.stringRep =>
            Types.Scalar(name, description)
          case TypeKinds.Object.stringRep =>
            Types.Object(name, description,
              readListT[Field](json, "fields"),
              readListT[Type](json, "interfaces")
            )
          case TypeKinds.Interface.stringRep =>
            Types.Interface(name, description,
              readListT[Field](json, "fields"),
              readListT[Type](json, "possibleTypes")
            )
          case TypeKinds.Union.stringRep =>
            Types.Union(name, description,
              readListT[Type](json, "possibleTypes")
            )
          case TypeKinds.Enum.stringRep =>
            Types.Enum(name, description,
              readListT[Type](json, "possibleTypes"),
              readListT[EnumValue](json, "enumValues")
            )
          case TypeKinds.InputObject.stringRep =>
            Types.InputObject(name, description,
              readListT[InputValue](json, "inputFields")
            )
          case TypeKinds.ListType.stringRep =>
            Types.ListType(name, description,
              readT[Type](json, "ofType")
            )
          case TypeKinds.NonNull.stringRep =>
            Types.NonNull(name, description,
              readT[Type](json, "ofType")
            )
        }
      }
    )

  implicit val fieldReadWrite: ReadWriter[Field] =
    readwriter[ujson.Value].bimap[Field](
      f => ujson.Obj(
        ("name", write[String](f.name)),
        ("description", write[Option[String]](f.description)),
        ("args", write[NonEmptyList[InputValue]](f.args)),
        ("type", write[Type](f.`type`)),
        ("isDeprecated", write[Boolean](f.isDeprecated)),
        ("deprecationReason", write[Option[String]](f.deprecationReason))
      ),
      json => {
        val name = readT[String](json, "name")
        val description = readOptionT[String](json, "description")
        val args = readT[NonEmptyList[InputValue]](json, "args")
        val tpe = readT[Type](json, "type")
        val isDeprecated = readT[Boolean](json, "isDeprecated")
        val deprecationReason = readOptionT[String](json, "deprecationReason")
        Field(name, description, args, tpe, isDeprecated, deprecationReason)
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
      json => InputValue(
        readT[String](json, "name"),
        readOptionT[String](json, "description"),
        readT[Type](json, "type"),
        readOptionT[String](json, "defaultValue"))
    )

  implicit val enumValueReadWrite: ReadWriter[EnumValue] =
    readwriter[ujson.Value].bimap[EnumValue](
      ev => ujson.Obj(
        ("name", write[String](ev.name)),
        ("description", write[Option[String]](ev.description)),
        ("isDeprecated", write[Boolean](ev.isDeprecated)),
        ("deprecationReason", write[Option[String]](ev.deprecationReason))
      ),
      json => {
        val name = read[String](json.obj("name"))
        val description = readOptionT[String](json, "description")
        val isDeprecated = readT[Boolean](json, "isDeprecated")
        val deprecationReason = readOptionT[String](json, "deprecationReason")
        EnumValue(name, description, isDeprecated, deprecationReason)
      }
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
      json => Directive(
        readT[String](json, "name"),
        readOptionT[String](json, "description"),
        readT[NonEmptyList[DirectiveLocation]](json, "locations"),
        readT[NonEmptyList[InputValue]](json, "args"))
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
  def parseIntrospection(json: String): Either[String, List[Schema]] = {
    try {
      Right(read[List[Schema]](json))
    } catch { case AbortException(clue, index, line, col, path, cause) =>
      Left(
        s"""Failed to parse on line $line, index: $index, column: $col.
           |Clue: $clue
           |Path: $path
           |""".stripMargin)
    }
  }
}
