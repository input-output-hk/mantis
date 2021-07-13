package io.iohk.ethereum.utils

import akka.util.ByteString
import boopickle.DefaultBasic._
import boopickle.Pickler
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{
  AccessListItem,
  Address,
  BlockBody,
  BlockHeader,
  Checkpoint,
  LegacyTransaction,
  SignedTransaction,
  Transaction,
  TransactionWithAccessList
}
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._

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
  implicit val accessListItemPickler: Pickler[AccessListItem] = generatePickler[AccessListItem]

  implicit val legacyTransactionPickler: Pickler[LegacyTransaction] = generatePickler[LegacyTransaction]
  implicit val transactionWithAccessListPickler: Pickler[TransactionWithAccessList] =
    generatePickler[TransactionWithAccessList]

  implicit val transactionPickler: Pickler[Transaction] = compositePickler[Transaction]
    .addConcreteType[LegacyTransaction]
    .addConcreteType[TransactionWithAccessList]

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
