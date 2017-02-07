package io.iohk.ethereum.domain

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

case class Account(
  nonce: BigInt,
  balance: BigInt,
  storageRoot: ByteString,
  codeHash: ByteString) {

  override def toString: String = {
    s"""Account{
         |nonce: $nonce
         |balance: $balance wei
         |storageRoot: ${Hex.toHexString(storageRoot.toArray[Byte])}
         |codeHash: ${Hex.toHexString(codeHash.toArray[Byte])}
         |}
       """.stripMargin
  }
}
