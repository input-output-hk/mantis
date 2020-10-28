package io.iohk.ethereum.rlp

import shapeless.{HList, HNil, Lazy, ::, LabelledGeneric, <:!<}
import shapeless.labelled.FieldType

/** Automatically derive RLP codecs for case classes. */
object RLPImplicitDerivations {

  /** Case classes get encoded as lists, not values,
    * which is an extra piece of information we want
    * to be able to rely on during derivation.
    */
  trait RLPListEncoder[T] extends RLPEncoder[T] {
    override def encode(obj: T): RLPList
  }
  object RLPListEncoder {
    def apply[T](f: T => RLPList): RLPListEncoder[T] =
      new RLPListEncoder[T] {
        override def encode(obj: T) = f(obj)
      }
  }

  /** Encoder for the empty list of fields. */
  implicit val deriveHNilRLPListEncoder: RLPListEncoder[HNil] =
    RLPListEncoder(_ => RLPList())

  /** Encoder that takes a list of fields which are the labelled generic
    * representation of a case class and turns it into an RLPList by
    * combining the RLP encoding of the head with the RLPList encoding of
    * the tail of the field list.
    *
    * This variant deals with trailing optional fields in the case classes,
    * which are omitted from the RLP list, instead of being as empty list items.
    */
  implicit def deriveOptionHListRLPListEncoder[K, H, T <: HList](implicit
      hEncoder: Lazy[RLPEncoder[H]],
      tEncoder: Lazy[RLPListEncoder[T]],
      ev: H <:< Option[_]
  ): RLPListEncoder[FieldType[K, H] :: T] = {
    // Create an encoder that takes a list of fields.
    RLPListEncoder { case head :: tail =>
      val tRLP = tEncoder.value.encode(tail)
      // If the fields is empty and the tail serialized to an RLPList is
      // also empty then it looks like a trailing field which is either
      // the last field of the class, or is followed by empty fields.
      if (head.isEmpty && tRLP.items.isEmpty)
        tRLP
      else {
        val hRLP = hEncoder.value.encode(head)
        hRLP :: tRLP
      }
    }
  }

  /** Deriving RLP encoding for a HList of fields where the current field is non-optional. */
  implicit def deriveNonOptionHListRLPListEncoder[K, H, T <: HList](implicit
      hEncoder: Lazy[RLPEncoder[H]],
      tEncoder: Lazy[RLPListEncoder[T]],
      ev: H <:!< Option[_]
  ): RLPListEncoder[FieldType[K, H] :: T] = {
    RLPListEncoder { case head :: tail =>
      val hRLP = hEncoder.value.encode(head)
      val tRLP = tEncoder.value.encode(tail)
      hRLP :: tRLP
    }
  }

  /** Derive an encoder for a case class based on its labelled generic record representation. */
  implicit def deriveLabelledGenericRLPListEncoder[T, Rec](implicit
      // Auto-derived by Shapeless.
      generic: LabelledGeneric.Aux[T, Rec],
      // Derived by `deriveHListRLPListEncoder`.
      recEncoder: Lazy[RLPListEncoder[Rec]]
  ): RLPListEncoder[T] = RLPListEncoder { value =>
    recEncoder.value.encode(generic.to(value))
  }

}
