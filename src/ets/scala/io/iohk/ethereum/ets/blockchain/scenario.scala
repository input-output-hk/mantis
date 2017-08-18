package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.common.AccountState


case class Scenario(
   blocks: List[BlockDef],
   genesisBlockHeader: BlockHeaderDef,
   genesisRLP: ByteString,
   lastblockhash: ByteString,
   network: String,
   postState: Map[Address, AccountState],
   pre: Map[Address, AccountState]
)


case class BlockDef(
  rlp: ByteString,
  blocknumber: BigInt,
  blockHeader: Option[BlockHeaderDef],
  transactions: Option[Seq[TransactionDef]],
  uncleHeaders: Option[Seq[BlockHeaderDef]]
)


case class TransactionDef(
  nonce: BigInt,
  gasPrice: BigInt,
  gasLimit: BigInt,
  to: Option[Address],
  value: BigInt,
  data:ByteString,
  r: BigInt,
  s: BigInt,
  v: ByteString
)

case class BlockHeaderDef(
  parentHash: ByteString,
  uncleHash: ByteString,
  coinbase: ByteString,
  stateRoot: ByteString,
  transactionsTrie: ByteString,
  receiptTrie: ByteString,
  bloom: ByteString,
  difficulty: BigInt,
  number: BigInt,
  gasLimit: BigInt,
  gasUsed: BigInt,
  timestamp: Long,
  extraData: ByteString,
  mixHash: ByteString,
  nonce: ByteString
) {

  def toBlockHeader: BlockHeader =
    BlockHeader(parentHash, uncleHash, coinbase, stateRoot, transactionsTrie, receiptTrie, bloom, difficulty, number,
      gasLimit, gasUsed, timestamp, extraData, mixHash, nonce
    )
}




