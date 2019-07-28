package com.jasonmartens.graphqlparser

import com.jasonmartens.graphqlparser.GraphQL._
import argonaut._
import Argonaut._
import argonaut.EncodeJsonScalaz._
import argonaut.DecodeJsonScalaz._

object IntrospectionParser {
  implicit def SchemaCodecJson: CodecJson[Schema] = {
    casecodec5(Schema.apply, Schema.unapply)("types", "queryType", "mutationType", "subscriptionType", "directives")
  }

  implicit def TypeScalarCodecJson: CodecJson[Types.Scalar] =
    casecodec2(Types.Scalar.apply, Types.Scalar.unapply)("name", "description")
  implicit def TypeObjectCodecJson: CodecJson[Types.Object] =
    casecodec4(Types.Object.apply, Types.Object.unapply)("name", "description", "fields", "interfaces")
  implicit def TypeInterfaceCodecJson: CodecJson[Types.Interface] =
    casecodec4(Types.Interface.apply, Types.Interface.unapply)("name", "description", "fields", "possibleTypes")
  implicit def TypeUnionCodecJson: CodecJson[Types.Union] =
    casecodec3(Types.Union.apply, Types.Union.unapply)("name", "description", "possibleTypes")
  implicit def TypeEnumCodecJson: CodecJson[Types.Enum] =
    casecodec4(Types.Enum.apply, Types.Enum.unapply)("name", "description", "possibleTypes", "enumValues")
  implicit def TypeInputObjectCodecJson: CodecJson[Types.InputObject] =
    casecodec3(Types.InputObject.apply, Types.InputObject.unapply)("name", "description", "inputFields")
  implicit def TypeListTypeCodecJson: CodecJson[Types.ListType] =
    casecodec3(Types.ListType.apply, Types.ListType.unapply)("name", "description", "ofType")
  implicit def TypeNonNullCodecJson: CodecJson[Types.NonNull] =
    casecodec3(Types.NonNull.apply, Types.NonNull.unapply)("name", "description", "ofType")
  implicit def TypeDecodeJson: DecodeJson[Type] = DecodeJson(c =>
  c.get[String]("kind") match {
    case DecodeResult(Left(errors)) =>
      DecodeResult[Type](Left(errors))
    case DecodeResult(Right(TypeKinds.Scalar.stringRep)) =>
      implicitly[DecodeJson[Types.Scalar]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.Object.stringRep)) =>
      implicitly[DecodeJson[Types.Object]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.Interface.stringRep)) =>
      implicitly[DecodeJson[Types.Interface]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.Union.stringRep)) =>
      implicitly[DecodeJson[Types.Union]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.Enum.stringRep)) =>
      implicitly[DecodeJson[Types.Enum]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.InputObject.stringRep)) =>
      implicitly[DecodeJson[Types.InputObject]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.ListType.stringRep)) =>
      implicitly[DecodeJson[Types.ListType]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(TypeKinds.NonNull.stringRep)) =>
      implicitly[DecodeJson[Types.NonNull]].decode(c).flatMap{DecodeResult.ok}
    case DecodeResult(Right(unknownString)) =>
      DecodeResult[Type](Left((s"Failed to find type for $unknownString", c.history)))
  })

  case object TypeEncodeJson extends EncodeJson[Type] {

    override def encode(gqlType: GraphQL.Type): Json = gqlType match {
      case t: Types.Scalar =>
        jencode3L((st: Types.Scalar) => (st.name, st.description, st.kind))(
          "name", "description", "kind").apply(t)
      case t: Types.Object =>
        jencode5L((ot: Types.Object) => (ot.name, ot.description, ot.fields, ot.interfaces, ot.kind))(
          "name", "description", "fields", "interfaces", "kind").apply(t)
      case t: Types.Interface =>
        jencode5L((it: Types.Interface) => (it.name, it.description, it.fields, it.possibleTypes, it.kind))(
          "name", "description", "fields", "possibleTypes", "kind").apply(t)
      case t: Types.Union =>
        jencode4L((ut: GraphQL.Types.Union) => (ut.name, ut.description, ut.possibleTypes, ut.kind))(
          "name", "description", "possibleTypes", "kind").apply(t)
      case t: Types.Enum =>
        jencode4L((et: GraphQL.Types.Enum) => (et.name, et.description, et.possibleTypes, et.kind))(
          "name", "description", "possibleTypes", "kind").apply(t)
      case t: Types.InputObject =>
        jencode4L((iot: GraphQL.Types.InputObject) => (iot.name, iot.description, iot.inputFields, iot.kind))(
          "name", "description", "inputFields", "kind").apply(t)
      case t: Types.ListType =>
        jencode4L((lt: GraphQL.Types.ListType) => (lt.name, lt.description, lt.ofType, lt.kind))(
          "name", "description", "ofType", "kind").apply(t)
      case t: Types.NonNull =>
        jencode4L((s: GraphQL.Types.NonNull) => (s.name, s.description, s.ofType, s.kind))(
          "name", "description", "ofType", "kind").apply(t)
      case _ => ???
    }
  }
  implicit val typeEncodeJson: EncodeJson[Type] = TypeEncodeJson

  implicit def FieldCodecJson: CodecJson[Field] = casecodec6(Field.apply, Field.unapply)("name", "description", "args", "type", "isDeprecated", "deprecationReason")

  implicit def InputValueCodecJson: CodecJson[InputValue] = casecodec4(InputValue.apply, InputValue.unapply)("name", "descrption", "type", "defaultValue")

  implicit def EnumValueCodecJson: CodecJson[EnumValue] = casecodec4(EnumValue.apply, EnumValue.unapply)("name", "description", "isDeprecated", "deprecationReason")

  implicit def DirectiveCodecJson: CodecJson[Directive] = {
    casecodec4(Directive.apply, Directive.unapply)("name", "description", "locations", "args")
  }

  implicit def TypeKindDecodeJson: DecodeJson[TypeKinds] = {
    DecodeJson(c => {c.as[String].map(tk => TypeKinds(tk))})
  }
  implicit def TypeKindEncodeJson: EncodeJson[TypeKinds] = {
    EncodeJson((tk: TypeKinds) => Json.jString(tk.stringRep))
  }

  implicit def DirectiveLocationDecodeJson: DecodeJson[DirectiveLocation] = {
    DecodeJson(c => {c.as[String].map(dl => DirectiveLocation(dl))})
  }
  implicit def DirectiveLocationEncodeJson: EncodeJson[DirectiveLocation] = {
    EncodeJson((dl: DirectiveLocation) => Json.jString(dl.stringRep))
  }


  def parse(json: String): Either[String, List[Schema]] = {
    Parse.decode[List[Schema]](json) match {
      case Right(schemaList) => Right(schemaList)
      case Left(Left(parseFailures)) => Left(parseFailures)
      case Left(Right((message, history))) => Left(s"Failed to decode: $message\nHistory: ${history}")
    }
  }
}
