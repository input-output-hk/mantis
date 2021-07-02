package io.iohk.ethereum.utils

import akka.util.ByteString

import boopickle.DefaultBasic._
import boopickle.Pickler

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._
import io.iohk.ethereum.domain.Checkpoint
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction

object Picklers {
  implicit val byteStringPickler: Pickler[ByteString] =
    transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val ecdsaSignaturePickler: Pickler[ECDSASignature] = generatePickler[ECDSASignature]
  implicit val checkpointPickler: Pickler[Checkpoint] = generatePickler[Checkpoint]

  implicit val hefPreEcip1098Pickler: Pickler[HefEmpty.type] = generatePickler[HefEmpty.type]
  implicit val hefPostEcip1097Pickler: Pickler[HefPostEcip1097] = generatePickler[HefPostEcip1097]

  implicit val extraFieldsPickler: Pickler[HeaderExtraFields] = compositePickler[HeaderExtraFields]
    .addConcreteType[HefPostEcip1097]
    .addConcreteType[HefEmpty.type]

  implicit val addressPickler: Pickler[Address] =
    transformPickler[Address, ByteString](bytes => Address(bytes))(address => address.bytes)
  implicit val transactionPickler: Pickler[Transaction] = generatePickler[Transaction]
  implicit val signedTransactionPickler: Pickler[SignedTransaction] =
    transformPickler[SignedTransaction, (Transaction, ECDSASignature)] { case (tx, signature) =>
      new SignedTransaction(tx, signature)
    }(stx => (stx.tx, stx.signature))

  implicit val blockHeaderPickler: Pickler[BlockHeader] = generatePickler[BlockHeader]
  implicit val blockBodyPickler: Pickler[BlockBody] =
    transformPickler[BlockBody, (Seq[SignedTransaction], Seq[BlockHeader])] { case (stx, nodes) =>
      BlockBody(stx, nodes)
    }(blockBody => (blockBody.transactionList, blockBody.uncleNodesList))
}
