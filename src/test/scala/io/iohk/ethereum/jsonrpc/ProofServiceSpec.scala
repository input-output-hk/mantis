package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.blockchain.BlockchainTestConfig
import io.iohk.ethereum.jsonrpc.ProofService.ProveAccountRequest
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MerklePatriciaTrie.AccountMPT
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

object ProofServiceSpec {
  val (testAddresses, testAccounts) = {
    // We will create accounts for these addresses and put them in the blockchain
    val addresses = IndexedSeq(
      "10F000",
      "101F00",
      "1012F0",
      "20000F"
    ).map(Hex.decode).map(Address.apply)

    val accounts =
      for {
        index ← 0 to addresses.length
        balance = index * 100 + 1
      } yield Account(balance = balance)

    (addresses, accounts)
  }

  private def newBlockchain(): BlockchainImpl = {
    val storagesInstance = new SharedEphemDataSources with PruningModeComponent with Storages.DefaultStorages {
      lazy val pruningMode: PruningMode = ArchivePruning
    }
    val storages = storagesInstance.storages

    BlockchainImpl(storages)
  }

  // Adds accounts to underlying storage and returns the new root hash
  private def addAccountsToStorage(blockchain: BlockchainImpl): NodeHash = {
    @tailrec
    def addAll(items: Iterator[(Address, Account)], mpt: AccountMPT): NodeHash = {
      if(items.hasNext) {
        val (address, account) = items.next()

        val newMpt = mpt.put(address, account)
        addAll(items, newMpt)
      }
      else ByteString(mpt.getRootHash)
    }

    val mpt0 = MerklePatriciaTrie.ofAccounts(blockchain)
    val addresses_accounts = testAddresses zip testAccounts
    addAll(addresses_accounts.iterator, mpt0)
  }

  // Updates blockhain with the new root hash (corresponding to added accounts)
  private def updateBlockchain(blockchain: BlockchainImpl, stateRoot: ByteString): Unit = {
    // Now that we have some state and a new state root,
    // let's fake a block that points to this new state.
    val blockSample = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val newHeader = blockSample.header.copy(stateRoot = stateRoot)
    val newBlock = blockSample.copy(header = newHeader)

    // Apparently this is not enough ...
    blockchain.save(newBlock)
    // ... so we also need to manually record the the block number
    blockchain.saveBestBlockNumber(newHeader.number)
  }

  private def createTestBlockchain(): BlockchainImpl = {
    val blockchain = newBlockchain()
    val stateRoot = addAccountsToStorage(blockchain)

    updateBlockchain(blockchain, stateRoot)

    blockchain
  }

  def createTestProofService(): ProofService = {
    val blockchainConfig = new BlockchainTestConfig {}
    val blockchain = createTestBlockchain()

    val proofService = new ProofService(
      blockchainConfig = blockchainConfig,
      blockchain = blockchain
    )

    proofService
  }
}

class ProofServiceSpec extends FlatSpec with Matchers with EitherValues with ScalaFutures {
  import ProofServiceSpec.{createTestProofService, testAccounts, testAddresses}

  "ProofService" should "prove account exists" in {
    val proofService = createTestProofService()
    val blockchain = proofService.blockchain

    val address = testAddresses(0)
    val account = testAccounts(0)

    val mpt = MerklePatriciaTrie.ofAccounts(blockchain)
    val accOpt = mpt.get(address)

    accOpt shouldNot be (None)
    accOpt.get shouldBe account

    val request = ProveAccountRequest(address)
    val rpcResult = proofService.proveAccount(request).futureValue

    val proofOpt = rpcResult.right.value.proofOpt
    proofOpt shouldNot be(None)
    val proof = proofOpt.get

    // Note that verification has also been done in
    // [[io.iohk.ethereum.mpt.MerklePatriciaTrieProofSuite MerklePatriciaTrieProofSuite]].
    // By doing it again here we just make sure that the implementation of
    // [[io.iohk.ethereum.jsonrpc.ProofService ProofService]]
    // does not do anything "fancy" (e.g. remove a [[ProofStep]]).
    val verified = mpt.verify(proof)

    verified shouldBe true
  }
}
