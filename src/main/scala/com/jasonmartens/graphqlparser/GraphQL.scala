package com.jasonmartens.graphqlparser

import scalaz.NonEmptyList

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
    val kind: TypeKinds
  }
  object Types {
    case class Scalar(
      name: Option[String],
      description: Option[String],
    ) extends Type {val kind: TypeKinds = TypeKinds.Scalar}

    case class Object(
      name: Option[String],
      description: Option[String],
      fields: List[Field],
      interfaces: List[Type]
    ) extends Type {val kind: TypeKinds = TypeKinds.Object}

    case class Interface(
      name: Option[String],
      description: Option[String],
      fields: List[Field],
      possibleTypes: List[Type]
    ) extends Type {val kind: TypeKinds = TypeKinds.Interface}

    case class Union(
      name: Option[String],
      description: Option[String],
      possibleTypes: List[Type]
    ) extends Type {val kind: TypeKinds = TypeKinds.Union}

    case class Enum(
      name: Option[String],
      description: Option[String],
      possibleTypes: List[Type],
      enumValues: List[EnumValue]
    ) extends Type {val kind: TypeKinds = TypeKinds.Enum}

    case class InputObject(
      name: Option[String],
      description: Option[String],
      inputFields: List[InputValue]
    ) extends Type {val kind: TypeKinds = TypeKinds.InputObject}

    case class ListType(
      name: Option[String],
      description: Option[String],
      ofType: Type
    ) extends Type {val kind: TypeKinds = TypeKinds.ListType}

    case class NonNull(
      name: Option[String],
      description: Option[String],
      ofType: Type
    ) extends Type {val kind: TypeKinds = TypeKinds.NonNull}

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

  sealed trait TypeKinds {val stringRep: String}
  object TypeKinds {
    def apply(kind: String): TypeKinds = kind match {
      case Scalar.`stringRep` => Scalar
      case Object.`stringRep` => Object
      case Interface.`stringRep` => Interface
      case Union.`stringRep` => Union
      case Enum.`stringRep` => Enum
      case InputObject.`stringRep` => InputObject
      case ListType.`stringRep` => ListType
      case NonNull.`stringRep` => NonNull
    }
    case object Scalar extends TypeKinds {val stringRep = "SCALAR"}
    case object Object extends TypeKinds {val stringRep = "OBJECT"}
    case object Interface extends TypeKinds {val stringRep = "INTERFACE"}
    case object Union extends TypeKinds {val stringRep = "UNION"}
    case object Enum extends TypeKinds {val stringRep = "ENUM"}
    case object InputObject extends TypeKinds {val stringRep = "INPUT_OBJECT"}
    case object ListType extends TypeKinds {val stringRep = "LIST"}
    case object NonNull extends TypeKinds {val stringRep = "NON_NULL"}
  }

  sealed trait DirectiveLocation {val stringRep: String}
  object DirectiveLocation {
    def apply(location: String): DirectiveLocation = location match {
      case Query.stringRep => Query
      case Mutation.stringRep => Mutation
      case Subscription.stringRep => Subscription
      case Field.stringRep => Field
      case FragmentDefinition.stringRep => FragmentDefinition
      case FragmentSpread.stringRep => FragmentSpread
      case InlineFragment.stringRep => InlineFragment
      case Schema.stringRep => Schema
      case Scalar.stringRep => Scalar
      case FieldDefinition.stringRep => FieldDefinition
      case ArgumentDefinition.stringRep => ArgumentDefinition
      case Interface.stringRep => Interface
      case Union.stringRep => Union
      case Enum.stringRep => Enum
      case EnumValue.stringRep => EnumValue
      case InputObject.stringRep => InputObject
      case InputFieldDefinition.stringRep => InputFieldDefinition
    }
    case object Query extends DirectiveLocation {val stringRep = "QUERY"}
    case object Mutation extends DirectiveLocation {val stringRep = "MUTATION"}
    case object Subscription extends DirectiveLocation {val stringRep = "SUBSCRIPTION"}
    case object Field extends DirectiveLocation {val stringRep = "FIELD"}
    case object FragmentDefinition extends DirectiveLocation {val stringRep = "FRAGMENT_DEFINITION"}
    case object FragmentSpread extends DirectiveLocation {val stringRep = "FRAGMENT_SPREAD"}
    case object InlineFragment extends DirectiveLocation {val stringRep = "INLINE_FRAGMENT"}
    case object Schema extends DirectiveLocation {val stringRep = "SCHEMA"}
    case object Scalar extends DirectiveLocation {val stringRep = "SCALAR"}
    case object Object extends DirectiveLocation {val stringRep = "OBJECT"}
    case object FieldDefinition extends DirectiveLocation {val stringRep = "FIELD_DEFINITION"}
    case object ArgumentDefinition extends DirectiveLocation {val stringRep = "ARGUMENT_DEFINITION"}
    case object Interface extends DirectiveLocation {val stringRep = "INTERFACE"}
    case object Union extends DirectiveLocation {val stringRep = "UNION"}
    case object Enum extends DirectiveLocation {val stringRep = "ENUM"}
    case object EnumValue extends DirectiveLocation {val stringRep = "ENUM_VALUE"}
    case object InputObject extends DirectiveLocation {val stringRep = "INPUT_OBJECT"}
    case object InputFieldDefinition extends DirectiveLocation {val stringRep = "INPUT_FIELD_DEFINITION"}
  }
}
