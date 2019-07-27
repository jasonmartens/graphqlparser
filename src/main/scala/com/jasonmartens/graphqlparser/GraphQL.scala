package com.jasonmartens.graphqlparser

import cats.data.NonEmptyList

object GraphQL {
  case class Schema (
    types: NonEmptyList[Type],
    queryType: Type,
    mutationType: Type,
    subscriptionType: Type,
    directives: NonEmptyList[Directive]
  )

  sealed trait Type {
    val name: Option[String]
    val description: Option[String]
    val kind: TypeKind
  }
  object Type {
    case class Scalar(
      name: Option[String],
      description: Option[String],
    ) extends Type {val kind: TypeKind = TypeKind.Scalar}

    case class Object(
      name: Option[String],
      description: Option[String],
      fields: List[Field],
      interfaces: List[Type]
    ) extends Type {val kind: TypeKind = TypeKind.Object}

    case class Interface(
      name: Option[String],
      description: Option[String],
      fields: List[Field],
      possibleTypes: List[Type]
    ) extends Type {val kind: TypeKind = TypeKind.Interface}

    case class Union(
      name: Option[String],
      description: Option[String],
      possibleTypes: List[Type]
    ) extends Type {val kind: TypeKind = TypeKind.Union}

    case class Enum(
      name: Option[String],
      description: Option[String],
      possibleTypes: List[Type],
      enumValues: List[EnumValue]
    ) extends Type {val kind: TypeKind = TypeKind.Enum}

    case class InputObject(
      name: Option[String],
      description: Option[String],
      inputFields: List[InputValue]
    ) extends Type {val kind: TypeKind = TypeKind.InputObject}

    case class ListType(
      name: Option[String],
      description: Option[String],
      ofType: Type
    ) extends Type {val kind: TypeKind = TypeKind.List}

    case class NonNull(
      name: Option[String],
      description: Option[String],
      ofType: Type
    ) extends Type {val kind: TypeKind = TypeKind.NonNull}

  }

  case class Field(
    name: String,
    description: Option[String],
    args: NonEmptyList[InputValue],
    `type`: Type,
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  case class InputValue(
    name: String,
    description: Option[String],
    `type`: Type,
    defaultValue: Option[String]
  )

  case class EnumValue (
    name: String,
    description: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  )

  case class Directive(
    name: String,
    description: Option[String],
    locations: NonEmptyList[DirectiveLocation],
    args: NonEmptyList[InputValue]
  )

  sealed trait TypeKind
  object TypeKind {
    case object Scalar extends TypeKind
    case object Object extends TypeKind
    case object Interface extends TypeKind
    case object Union extends TypeKind
    case object Enum extends TypeKind
    case object InputObject extends TypeKind
    case object List extends TypeKind
    case object NonNull extends TypeKind
  }

  sealed trait DirectiveLocation
  object DirectiveLocation {
    case object Query extends DirectiveLocation
    case object Mutation extends DirectiveLocation
    case object Subscription extends DirectiveLocation
    case object Field extends DirectiveLocation
    case object FragmentDefinition extends DirectiveLocation
    case object FragmentSpread extends DirectiveLocation
    case object InlineFragment extends DirectiveLocation
    case object Schema extends DirectiveLocation
    case object Scalar extends DirectiveLocation
    case object Object extends DirectiveLocation
    case object FieldDefinition extends DirectiveLocation
    case object ArgumentDefinition extends DirectiveLocation
    case object Interface extends DirectiveLocation
    case object Union extends DirectiveLocation
    case object Enum extends DirectiveLocation
    case object EnumValue extends DirectiveLocation
    case object InputObject extends DirectiveLocation
    case object InputFieldDefinition extends DirectiveLocation
  }
}
