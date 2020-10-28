package io.iohk.ethereum.rlp

import shapeless.{HList, HNil, Lazy, ::, LabelledGeneric, <:!<}
import shapeless.labelled.FieldType

/** Automatically derive RLP codecs for case classes. */
object RLPImplicitDerivations {

  /** Support introspecting on what happened during encoding the tail. */
  case class FieldInfo(isOptional: Boolean)

  /** Case classes get encoded as lists, not values,
    * which is an extra piece of information we want
    * to be able to rely on during derivation.
    */
  trait RLPListEncoder[T] extends RLPEncoder[T] {
    def encodeList(obj: T): (RLPList, List[FieldInfo])

    override def encode(obj: T): RLPEncodeable =
      encodeList(obj)._1
  }
  object RLPListEncoder {
    def apply[T](f: T => (RLPList, List[FieldInfo])): RLPListEncoder[T] =
      new RLPListEncoder[T] {
        override def encodeList(obj: T) = f(obj)
      }
  }

  /** Encoder for the empty list of fields. */
  implicit val deriveHNilRLPListEncoder: RLPListEncoder[HNil] =
    RLPListEncoder(_ => RLPList() -> Nil)

  /** Encoder that takes a list of fields which are the labelled generic
    * representation of a case class and turns it into an RLPList by
    * combining the RLP encoding of the head with the RLPList encoding of
    * the tail of the field list.
    *
    * This variant deals with trailing optional fields in the case classes,
    * which can be omitted from the RLP list, instead of being added as empty lists.
    */
  implicit def deriveOptionHListRLPListEncoder[K, H, T <: HList](implicit
      hEncoder: Lazy[RLPEncoder[H]],
      tEncoder: Lazy[RLPListEncoder[T]],
      ev: H <:< Option[_]
  ): RLPListEncoder[FieldType[K, H] :: T] = {
    val hInfo = FieldInfo(isOptional = true)
    // Create an encoder that takes a list of field values.
    RLPListEncoder { case head :: tail =>
      val (tRLP, tInfos) = tEncoder.value.encodeList(tail)
      val htRLP =
        if (tInfos.forall(_.isOptional)) {
          // This is still a trailing optional field, so we can insert it as a value or omit it.
          hEncoder.value.encode(head) match {
            case RLPList(hRLP) =>
              hRLP :: tRLP
            case RLPList() if tRLP.items.isEmpty =>
              tRLP
            case hRLP =>
              hRLP :: tRLP
          }
        } else {
          // We're no longer in a trailing position, so insert it as a list of 0 or 1 items.
          hEncoder.value.encode(head) :: tRLP
        }

      htRLP -> (hInfo :: tInfos)
    }
  }

  /** Deriving RLP encoding for a HList of fields where the current field is non-optional. */
  implicit def deriveNonOptionHListRLPListEncoder[K, H, T <: HList](implicit
      hEncoder: Lazy[RLPEncoder[H]],
      tEncoder: Lazy[RLPListEncoder[T]],
      ev: H <:!< Option[_]
  ): RLPListEncoder[FieldType[K, H] :: T] = {
    val hInfo = FieldInfo(isOptional = false)
    RLPListEncoder { case head :: tail =>
      val hRLP = hEncoder.value.encode(head)
      val (tRLP, tInfos) = tEncoder.value.encodeList(tail)
      (hRLP :: tRLP, hInfo :: tInfos)
    }
  }

  /** Derive an encoder for a case class based on its labelled generic record representation. */
  implicit def deriveLabelledGenericRLPListEncoder[T, Rec](implicit
      // Auto-derived by Shapeless.
      generic: LabelledGeneric.Aux[T, Rec],
      // Derived by `deriveOptionHListRLPListEncoder` and `deriveNonOptionHListRLPListEncoder`.
      recEncoder: Lazy[RLPListEncoder[Rec]]
  ): RLPListEncoder[T] = RLPListEncoder { value =>
    recEncoder.value.encodeList(generic.to(value))
  }

}
