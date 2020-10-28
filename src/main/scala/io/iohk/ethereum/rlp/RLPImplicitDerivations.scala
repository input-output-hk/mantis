package io.iohk.ethereum.rlp

import shapeless.{HList, HNil, Lazy, ::, LabelledGeneric, <:!<, Witness}
import shapeless.labelled.{FieldType, field}
import scala.util.control.NonFatal
import scala.reflect.ClassTag

/** Automatically derive RLP codecs for case classes. */
object RLPImplicitDerivations {

  case class DerivationPolicy(
      // Whether to treat optional fields at the end of the list like
      // they can be omitted from the RLP list, or inserted as a value,
      // as opposed to a list of 0 or 1 items.
      omitTrailingOptionals: Boolean
  )

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

  /** Specialized decoder for case classes that only accepts RLPList for input. */
  trait RLPListDecoder[T] extends RLPDecoder[T] {
    def decodeList(items: List[RLPEncodeable]): (T, List[FieldInfo])

    override def decode(rlp: RLPEncodeable): T =
      rlp match {
        case list: RLPList =>
          decodeList(list.items.toList)._1
        case other =>
          throw new RuntimeException("Expected to decode an RLPList.")
      }
  }
  object RLPListDecoder {
    def apply[T](f: List[RLPEncodeable] => (T, List[FieldInfo])): RLPListDecoder[T] =
      new RLPListDecoder[T] {
        override def decodeList(items: List[RLPEncodeable]) = f(items)
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
      ev: H <:< Option[_],
      policy: DerivationPolicy
  ): RLPListEncoder[FieldType[K, H] :: T] = {
    val hInfo = FieldInfo(isOptional = true)
    // Create an encoder that takes a list of field values.
    RLPListEncoder { case head :: tail =>
      val (tRLP, tInfos) = tEncoder.value.encodeList(tail)
      val htRLP =
        if (policy.omitTrailingOptionals && tInfos.forall(_.isOptional)) {
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

  /** Encoder for a HList of fields where the current field is non-optional. */
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

  /** Encoder for a case class based on its labelled generic record representation. */
  implicit def deriveLabelledGenericRLPEncoder[T, Rec](implicit
      // Auto-derived by Shapeless.
      generic: LabelledGeneric.Aux[T, Rec],
      // Derived by `deriveOptionHListRLPListEncoder` and `deriveNonOptionHListRLPListEncoder`.
      recEncoder: Lazy[RLPEncoder[Rec]]
  ): RLPEncoder[T] = RLPEncoder { value =>
    recEncoder.value.encode(generic.to(value))
  }

  /** Decoder for the empty list of fields.
    *
    * We can ignore extra items in the RLPList as optional fields we don't handle,
    * or extra random data, which we have for example in EIP8 test vectors.
    */
  implicit val deriveHNilRLPListDecoder: RLPListDecoder[HNil] =
    RLPListDecoder(_ => HNil -> Nil)

  /** Decoder for a list of fields in the generic represenation of a case class.
    *
    * This variant deals with trailing optional fields, which may be omitted from
    * the end of RLP lists.
    */
  implicit def deriveOptionHListRLPListDecoder[K <: Symbol, H, V, T <: HList](implicit
      hDecoder: Lazy[RLPDecoder[H]],
      tDecoder: Lazy[RLPListDecoder[T]],
      // The witness provides access to the Symbols which LabelledGeneric uses
      // to tag the fields with their names, so we can use it to provide better
      // contextual error messages.
      witness: Witness.Aux[K],
      ev: Option[V] =:= H,
      policy: DerivationPolicy
  ): RLPListDecoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    val hInfo = FieldInfo(isOptional = true)

    RLPListDecoder {
      case Nil if policy.omitTrailingOptionals =>
        val (tail, tInfos) = tDecoder.value.decodeList(Nil)
        val value: H = None
        val head: FieldType[K, H] = field[K](value)
        (head :: tail) -> (hInfo :: tInfos)

      case Nil =>
        throw new RuntimeException(s"Cannot decode optional '${fieldName}': the RLPList is empty.")

      case rlps =>
        val (tail, tInfos) = tDecoder.value.decodeList(rlps.tail)
        val value: H =
          try {
            if (policy.omitTrailingOptionals && tInfos.forall(_.isOptional)) {
              // Treat it as a value. We have a decoder for optional fields, so we have to wrap it.
              hDecoder.value.decode(RLPList(rlps.head))
            } else {
              hDecoder.value.decode(rlps.head)
            }
          } catch {
            case NonFatal(ex) =>
              throw new RuntimeException(s"Cannot decode optional '$fieldName' from RLP value: ${ex.getMessage}")
          }

        val head: FieldType[K, H] = field[K](value)
        (head :: tail) -> (hInfo :: tInfos)
    }
  }

  /** Decoder for a non-optional field. */
  implicit def deriveNonOptionHListRLPListDecoder[K <: Symbol, H, T <: HList](implicit
      hDecoder: Lazy[RLPDecoder[H]],
      tDecoder: Lazy[RLPListDecoder[T]],
      witness: Witness.Aux[K],
      ev: H <:!< Option[_]
  ): RLPListDecoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    val hInfo = FieldInfo(isOptional = false)

    RLPListDecoder {
      case Nil =>
        throw new RuntimeException(s"Cannot decode '${fieldName}': the RLPList is empty.")

      case rlps =>
        val value: H =
          try {
            hDecoder.value.decode(rlps.head)
          } catch {
            case NonFatal(ex) =>
              throw new RuntimeException(s"Cannot decode '$fieldName' from RLP value: ${ex.getMessage}")
          }
        val head: FieldType[K, H] = field[K](value)
        val (tail, tInfos) = tDecoder.value.decodeList(rlps.tail)
        (head :: tail) -> (hInfo :: tInfos)
    }
  }

  /** Decoder for a case class based on its labelled generic record representation. */
  implicit def deriveLabelledGenericRLPDecoder[T, Rec](implicit
      // Auto-derived by Shapeless.
      generic: LabelledGeneric.Aux[T, Rec],
      // Derived by `deriveOptionHListRLPListDecoder` and `deriveNonOptionHListRLPListDecoder`.
      recDecoder: Lazy[RLPDecoder[Rec]],
      ct: ClassTag[T]
  ): RLPDecoder[T] = RLPDecoder { rlp =>
    try {
      generic.from(recDecoder.value.decode(rlp))
    } catch {
      case NonFatal(ex) =>
        throw new RuntimeException(s"Could not decode ${ct.runtimeClass.getSimpleName}: ${ex.getMessage}")
    }
  }

  /** Derive both encoder and decoder. */
  implicit def deriveLabelledGenericRLPCodec[T, Rec](implicit
      generic: LabelledGeneric.Aux[T, Rec],
      recEncoder: Lazy[RLPEncoder[Rec]],
      recDecoder: Lazy[RLPDecoder[Rec]],
      ct: ClassTag[T]
  ): RLPCodec[T] =
    RLPCodec[T](deriveLabelledGenericRLPEncoder, deriveLabelledGenericRLPDecoder)
}
