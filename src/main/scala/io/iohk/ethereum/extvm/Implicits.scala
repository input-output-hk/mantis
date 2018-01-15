package io.iohk.ethereum.extvm

import language.implicitConversions

import akka.util.ByteString
import com.google.protobuf.{ByteString => GByteString}
import io.iohk.ethereum.domain.{Address, UInt256}

object Implicits {
  implicit def byteString2GByteString(b: ByteString): GByteString =
    GByteString.copyFrom(b.toArray)

  implicit def address2GByteString(a: Address): GByteString =
    GByteString.copyFrom(a.toArray)

  implicit def uint256ToGByteString(u: UInt256): GByteString =
    GByteString.copyFrom(u.bytes.toArray)

  implicit def bigintToGByteString(b: BigInt): GByteString =
    UInt256(b)

  implicit def byteStringFromGByteString(gb: GByteString): ByteString =
    ByteString(gb.toByteArray)

  implicit def addressFromGByteString(gb: GByteString): Address =
    Address(gb.toByteArray)

  implicit def uint256FromGByteString(gb: GByteString): UInt256 =
    UInt256(gb.toByteArray)

  implicit def bigintFromGByteString(gb: GByteString): BigInt =
    gb.toBigInt
}
