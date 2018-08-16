package io.iohk.ethereum.blockchain.visitor
package app

import java.time.{ZoneOffset, ZonedDateTime}

import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.nodebuilder.{BlockchainBuilder, StorageBuilder}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.UInt256RLPImplicits._
import io.iohk.ethereum.utils.{BlockchainConfig, JsonUtils}

import scala.collection.mutable.ListBuffer

case class Counters(
  detailsLength: Long,
  blocks: Long,
  txs: Long,
  contracts: Long,
  emptyContractCreationPayloads: Long,
  contractCreationPayloadsSize: Long,
  emptySmartContracts: Long,   // no code
  emptyContractAccounts: Long, // getAccount() = Some(Account.empty())
  noContractAccounts: Long     // getAccount() = None
)

case class BlockInfo(
  number: BigInt,
  numberHex: String,
  hash: String,
  timestamp: Long,
  timestampUTC: String,
  difficulty: BigInt,
  gasLimit: BigInt,
  gasUsed: BigInt
)

case class TxInfo(
  senderAddress: String,
  signedTxHash: String,
  txIndex: Int,
  nonce: BigInt,
  gasPrice: BigInt,
  gasLimit: BigInt,
  value: BigInt,
  payloadSize: Int
)

case class ContractDetails(
  blockInfo: BlockInfo,
  txInfo: TxInfo,
  contractCreationPayloadSize: Int,
  smartContractCodeSize: Int,
  noAccount: Boolean, // getAccount() = None
  emptyAccount: Boolean // getAccount() = Some(Account.empty())
)

case class Result(
  details: List[ContractDetails],
  accountStartNonce: String,
  counters: Counters
)

class CountersListener(blockchain: Blockchain, blockchainConfig: BlockchainConfig) extends BlockchainListener[Result] {
  private[this] var _blocks = 0L
  private[this] var _txs = 0L
  private[this] var _contracts = 0L
  private[this] var _emptyContractCreationPayloads = 0L
  private[this] var _contractCreationPayloadsSize = 0L
  private[this] var _emptySmartContracts = 0L
  private[this] var _emptyContractAccounts = 0L
  private[this] var _noContractAccounts = 0L
  private[this] var _contractDetails = ListBuffer[ContractDetails]()

  def result: Result = {
    val counters = Counters(
      detailsLength = _contractDetails.length,
      blocks = _blocks,
      txs = _txs,
      contracts = _contracts,
      emptyContractCreationPayloads = _emptyContractCreationPayloads,
      contractCreationPayloadsSize = _contractCreationPayloadsSize,
      emptySmartContracts = _emptySmartContracts,
      emptyContractAccounts = _emptyContractAccounts,
      noContractAccounts = _noContractAccounts
    )

    Result(
      details = _contractDetails.toList,
      accountStartNonce = blockchainConfig.accountStartNonce.toString,
      counters = counters
    )
  }

  override def leaveBlock(block: Block): Unit =
    _blocks += 1

  override def leaveTransaction(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit =
    _txs += 1

  // scalastyle:off method.length
  override def leaveContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {
    val unixTimestamp: Long = block.header.unixTimestamp
    val unixTimestampUTC = ZonedDateTime.ofInstant(java.time.Instant.ofEpochSecond(unixTimestamp), ZoneOffset.UTC)
    val tx = stx.tx
    val txPayloadSize = tx.payload.size

    _contracts += 1
    _contractCreationPayloadsSize += txPayloadSize
    if(txPayloadSize == 0) { _emptyContractCreationPayloads += 1 }

    def updateDetails(noAccount: Boolean, emptyAccount: Boolean, smartContractCodeSize: Int): Unit =
      _contractDetails += ContractDetails(
        blockInfo = BlockInfo(
          number = block.header.number,
          numberHex = "0x" + block.header.number.toString(16),
          hash = "0x" + block.header.hashAsHexString,
          timestamp = unixTimestamp,
          timestampUTC = unixTimestampUTC.toString,
          difficulty = block.header.difficulty,
          gasLimit = block.header.gasLimit,
          gasUsed = block.header.gasUsed
        ),
        txInfo = TxInfo(
          senderAddress = stx.senderAddress.toString,
          signedTxHash = "0x" + stx.hashAsHexString,
          txIndex = stxi,
          nonce = tx.nonce,
          gasPrice = tx.gasPrice,
          gasLimit = tx.gasLimit,
          value = tx.value,
          payloadSize = tx.payload.size
        ),
        contractCreationPayloadSize = txPayloadSize,
        smartContractCodeSize = smartContractCodeSize,
        noAccount = noAccount,
        emptyAccount = emptyAccount
      )

    val accountAddressHash = kec256(rlp.encode(RLPList(stx.senderAddress.bytes, UInt256(tx.nonce).toRLPEncodable)))
    val accountAddress = Address(accountAddressHash)

    // Find account of the smart contract
    val bestBlockNumber = blockchain.getBestBlockNumber()
    val bestBlockOpt = blockchain.getBlockByNumber(bestBlockNumber)
    for(bestBlock ← bestBlockOpt) {
      val header: BlockHeader = bestBlock.header
      val world = blockchain.getWorldStateProxy(
        header.number,
        blockchainConfig.accountStartNonce,
        Some(header.stateRoot),
        noEmptyAccounts = false,
        ethCompatibilityMode = blockchainConfig.ethCompatibilityMode
      )

      // Check account emptiness/existence
      val contractAccountOpt = world.getAccount(accountAddress)
      val (noAccount, emptyAccount) =
        contractAccountOpt match {
          case None ⇒
            _noContractAccounts += 1
            (true, false)

          case Some(contractAccount) ⇒
            if(contractAccount.isEmpty(blockchainConfig.accountStartNonce)) {
              _emptyContractAccounts += 1

              (false, true)
            }
            else (false, false)
        }

      val code = world.getCode(accountAddress)
      if(code.isEmpty) { _emptySmartContracts += 1 }

      if(noAccount || emptyAccount) {
        updateDetails(noAccount = noAccount, emptyAccount = emptyAccount, smartContractCodeSize = code.size)
      }
    }
  }
}

object FindEmptyAccounts {
  def main(args: Array[String]): Unit = {
    val builder = new BlockchainBuilder with StorageBuilder
    val blockchain = builder.blockchain
    val blockchainConfig = builder.storagesInstance.blockchainConfig

    val listener = new CountersListener(blockchain, blockchainConfig)
    val visitor = new FullBlockchainVisitor(listener)
    val result = visitor.run(blockchain)
    val resultJson = JsonUtils.pretty(result)

    // scalastyle:off
    println(resultJson)

    builder.storagesInstance.dataSources.closeAll()
  }
}
