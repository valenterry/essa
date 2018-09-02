package essa

/**
  * Tags a type with another type, describing that the tagged type originated from the type tag, that is, Value was created from OrigType
  */
trait OTag[OriginalType, +Type]
object OTagged {
  type OT[OriginalType, +Type] = Type with OTag[OriginalType, Type]
}
