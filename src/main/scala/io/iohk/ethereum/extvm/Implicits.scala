package io.iohk.ethereum.extvm

import akka.util.ByteString

import com.google.protobuf.{ByteString => GByteString}

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256

import language.implicitConversions

object Implicits {
  implicit def byteString2GByteString(b: ByteString): GByteString =
    GByteString.copyFrom(b.toArray)

  implicit def address2GByteString(a: Address): GByteString =
    GByteString.copyFrom(a.toArray)

  implicit def uint256ToGByteString(u: UInt256): GByteString =
    u.toBigInt

  implicit def bigintToGByteString(b: BigInt): GByteString =
    GByteString.copyFrom(b.toByteArray)

  implicit def byteStringFromGByteString(gb: GByteString): ByteString =
    ByteString(gb.toByteArray)

  implicit def addressFromGByteString(gb: GByteString): Address =
    Address(gb.toByteArray)

  implicit def uint256FromGByteString(gb: GByteString): UInt256 =
    UInt256(gb: BigInt)

  implicit def bigintFromGByteString(gb: GByteString): BigInt =
    BigInt(gb.toByteArray)
}
