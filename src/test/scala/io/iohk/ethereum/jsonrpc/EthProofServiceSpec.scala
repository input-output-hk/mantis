package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString

import monix.execution.Scheduler.Implicits.global

import com.softwaremill.diffx.scalatest.DiffMatcher
import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGenerator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthUserService.GetBalanceRequest
import io.iohk.ethereum.jsonrpc.EthUserService.GetBalanceResponse
import io.iohk.ethereum.jsonrpc.EthUserService.GetStorageAtRequest
import io.iohk.ethereum.jsonrpc.EthUserService.GetTransactionCountRequest
import io.iohk.ethereum.jsonrpc.ProofService.GetProofRequest
import io.iohk.ethereum.jsonrpc.ProofService.StorageProofKey
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MerklePatriciaTrie.defaultByteArraySerializable
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.rlp.RLPValue

class EthProofServiceSpec
    extends TestKit(ActorSystem("EthGetProofSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals
    with DiffMatcher {

  "EthProofService" should "handle getStorageAt request" in new TestSetup {
    val request = GetProofRequest(address, storageKeys, blockNumber)
    val result = ethGetProof.getProof(request)

    val balanceResponse: GetBalanceResponse = ethUserService
      .getBalance(GetBalanceRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethUserService.getBalance did not get valid response"))

    val transactionCountResponse = ethUserService
      .getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethUserService.getTransactionCount did not get valid response"))

    val storageValues: Seq[ByteString] = storageKeys.map { position =>
      ethUserService
        .getStorageAt(GetStorageAtRequest(address, position.v, BlockParam.Latest))
        .runSyncUnsafe()
        .getOrElse(fail("ethUserService.getStorageAt did not get valid response"))
        .value
    }

    val givenResult = result
      .runSyncUnsafe()
      .getOrElse(fail())
      .proofAccount

    givenResult.address should matchTo(address)
    givenResult.codeHash shouldBe account.codeHash
    givenResult.storageHash shouldBe account.storageRoot

    givenResult.nonce shouldBe UInt256(transactionCountResponse.value)

    givenResult.balance shouldBe balanceResponse.value

    givenResult.storageProof.map(_.key) shouldBe storageKeys
    givenResult.storageProof.map(_.value.toString) shouldBe storageValues.map(_.mkString)
    givenResult.storageProof.map(_.proof).foreach { p =>
      p should not be empty
    }
  }

  "EthProofService" should "return an error when the proof is requested for non-existing account" in new TestSetup {
    val wrongAddress = Address(666)
    val result = fetchProof(wrongAddress, storageKeys, blockNumber).runSyncUnsafe()
    result.isLeft shouldBe true
    result.fold(l => l.message should include("No account found for Address"), r => r)
  }

  "EthProofService" should "return the proof with empty value for non-existing storage key" in new TestSetup {
    val wrongStorageKey = Seq(StorageProofKey(321))
    val result = fetchProof(address, wrongStorageKey, blockNumber).runSyncUnsafe()
    result.isRight shouldBe true
    result.fold(
      l => l,
      r => {
        val accountProof = r.proofAccount
        accountProof.address should matchTo(address)
        accountProof.accountProof.foreach { p =>
          p should not be empty
        }
        accountProof.accountProof.head shouldBe rlp.encode(RLPValue(mpt.getRootHash))
        accountProof.balance shouldBe balance.toBigInt
        accountProof.codeHash shouldBe account.codeHash
        accountProof.nonce shouldBe nonce
        accountProof.storageHash shouldBe account.storageRoot
        accountProof.storageProof.map { v =>
          v.proof.nonEmpty shouldBe true
          v.value shouldBe BigInt(0)
        }
      }
    )
  }

  "EthProofService" should "return the proof and value for existing storage key" in new TestSetup {
    val storageKey = Seq(StorageProofKey(key))
    val result = fetchProof(address, storageKey, blockNumber).runSyncUnsafe()
    result.isRight shouldBe true
    result.fold(
      l => l,
      r => {
        val accountProof = r.proofAccount
        accountProof.address should matchTo(address)
        accountProof.accountProof.foreach { p =>
          p should not be empty
        }
        accountProof.accountProof.head shouldBe rlp.encode(RLPValue(mpt.getRootHash))
        accountProof.balance shouldBe balance.toBigInt
        accountProof.codeHash shouldBe account.codeHash
        accountProof.nonce shouldBe nonce
        accountProof.storageHash shouldBe account.storageRoot
        r.proofAccount.storageProof.map { v =>
          v.proof.nonEmpty shouldBe true
          v.value shouldBe BigInt(value)
        }
      }
    )
  }

  "EthProofService" should "return the proof and value for multiple existing storage keys" in new TestSetup {
    val storageKey = Seq(StorageProofKey(key), StorageProofKey(key2))
    val expectedValueStorageKey = Seq(BigInt(value), BigInt(value2))
    val result = fetchProof(address, storageKey, blockNumber).runSyncUnsafe()
    result.isRight shouldBe true
    result.fold(
      l => l,
      r => {
        val accountProof = r.proofAccount
        accountProof.address should matchTo(address)
        accountProof.accountProof.foreach { p =>
          p should not be empty
        }
        accountProof.accountProof.head shouldBe rlp.encode(RLPValue(mpt.getRootHash))
        accountProof.balance shouldBe balance.toBigInt
        accountProof.codeHash shouldBe account.codeHash
        accountProof.nonce shouldBe nonce
        accountProof.storageHash shouldBe account.storageRoot
        accountProof.storageProof.size shouldBe 2
        accountProof.storageProof.map { v =>
          v.proof.nonEmpty shouldBe true
          expectedValueStorageKey should contain(v.value)
        }
      }
    )
  }

  "EthProofService" should "return the proof for all storage keys provided, but value should be returned only for the existing ones" in new TestSetup {
    val wrongStorageKey = StorageProofKey(321)
    val storageKey = Seq(StorageProofKey(key), StorageProofKey(key2)) :+ wrongStorageKey
    val expectedValueStorageKey = Seq(BigInt(value), BigInt(value2), BigInt(0))
    val result = fetchProof(address, storageKey, blockNumber).runSyncUnsafe()
    result.isRight shouldBe true
    result.fold(
      l => l,
      r => {
        val accountProof = r.proofAccount
        accountProof.address should matchTo(address)
        accountProof.accountProof.foreach { p =>
          p should not be empty
        }
        accountProof.accountProof.head shouldBe rlp.encode(RLPValue(mpt.getRootHash))
        accountProof.balance shouldBe balance.toBigInt
        accountProof.codeHash shouldBe account.codeHash
        accountProof.nonce shouldBe nonce
        accountProof.storageHash shouldBe account.storageRoot
        accountProof.storageProof.size shouldBe 3
        expectedValueStorageKey.forall(accountProof.storageProof.map(_.value).contains) shouldBe true
      }
    )
  }

  "EthProofService" should "return account proof and account details, with empty storage proof" in new TestSetup {
    val result = fetchProof(address, Seq.empty, blockNumber).runSyncUnsafe()
    result.isRight shouldBe true
    result.fold(
      l => l,
      r => {
        val accountProof = r.proofAccount
        accountProof.address should matchTo(address)
        accountProof.accountProof.foreach { p =>
          p should not be empty
        }
        accountProof.accountProof.head shouldBe rlp.encode(RLPValue(mpt.getRootHash))
        accountProof.balance shouldBe balance.toBigInt
        accountProof.codeHash shouldBe account.codeHash
        accountProof.nonce shouldBe nonce
        accountProof.storageHash shouldBe account.storageRoot
        accountProof.storageProof.size shouldBe 0
      }
    )
  }

  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {

    val blockGenerator: PoWBlockGenerator = mock[PoWBlockGenerator]
    val address: Address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))
    val balance: UInt256 = UInt256(0)
    val nonce = 0

    val key = 333
    val value = 123
    val key1 = 334
    val value1 = 124
    val key2 = 335
    val value2 = 125

    val storageMpt: MerklePatriciaTrie[BigInt, BigInt] = EthereumUInt256Mpt
      .storageMpt(
        ByteString(MerklePatriciaTrie.EmptyRootHash),
        storagesInstance.storages.stateStorage.getBackingStorage(0)
      )
      .put(UInt256(key), UInt256(value))
      .put(UInt256(key1), UInt256(value1))
      .put(UInt256(key2), UInt256(value2))

    val account: Account = Account(
      nonce = nonce,
      balance = balance,
      storageRoot = ByteString(storageMpt.getRootHash)
    )

    val mpt: MerklePatriciaTrie[Array[Byte], Account] =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          account
        )

    val blockToRequest: Block = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val newBlockHeader: BlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock: Block = blockToRequest.copy(header = newBlockHeader)
    blockchainWriter.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.hash, newblock.number)

    val ethGetProof =
      new EthProofService(blockchain, blockchainReader, blockGenerator, blockchainConfig.ethCompatibleStorage)

    val storageKeys: Seq[StorageProofKey] = Seq(StorageProofKey(key))
    val blockNumber = BlockParam.Latest

    def fetchProof(
        address: Address,
        storageKeys: Seq[StorageProofKey],
        blockNumber: BlockParam
    ): ServiceResponse[ProofService.GetProofResponse] = {
      val request = GetProofRequest(address, storageKeys, blockNumber)
      val retrievedAccountProof: ServiceResponse[ProofService.GetProofResponse] = ethGetProof.getProof(request)
      retrievedAccountProof
    }

    val ethUserService = new EthUserService(
      blockchain,
      blockchainReader,
      mining,
      storagesInstance.storages.evmCodeStorage,
      this
    )
  }
}
