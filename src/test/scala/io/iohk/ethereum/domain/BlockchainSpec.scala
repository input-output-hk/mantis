package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.{Fixtures, crypto}
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import org.scalatest.{FlatSpec, Matchers}

class BlockchainSpec extends FlatSpec with Matchers {

  "Blockchain" should "be able to store a block and return if if queried by hash" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
    val block = blockchain.getBlockByHash(validBlock.header.hash)
    assert(block.isDefined)
    assert(validBlock == block.get)
    val blockHeader = blockchain.getBlockHeaderByHash(validBlock.header.hash)
    assert(blockHeader.isDefined)
    assert(validBlock.header == blockHeader.get)
    val blockBody = blockchain.getBlockBodyByHash(validBlock.header.hash)
    assert(blockBody.isDefined)
    assert(validBlock.body == blockBody.get)
  }

  it should "be able to store a block and retrieve it by number" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
    val block = blockchain.getBlockByNumber(validBlock.header.number)
    assert(block.isDefined)
    assert(validBlock == block.get)
  }

  it should "be able to query a stored blockHeader by it's number" in new EphemBlockchainTestSetup {
    val validHeader = Fixtures.Blocks.ValidBlock.header
    blockchain.save(validHeader)
    val header = blockchain.getBlockHeaderByNumber(validHeader.number)
    assert(header.isDefined)
    assert(validHeader == header.get)
  }

  it should "not return a value if not stored" in new EphemBlockchainTestSetup {
    assert(blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.header.number).isEmpty)
    assert(blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash).isEmpty)
  }

  it should "return an account given an address and a block number" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.Empty.copy(nonce = 7)

    val validHeader = Fixtures.Blocks.ValidBlock.header

    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.nodeStorage,
      crypto.kec256(_: Array[Byte])
    )

    val mptWithAcc = emptyMpt.put(address, account)
    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchain.save(headerWithAcc)

    val retrievedAccount = blockchain.getAccount(address, headerWithAcc.number)
    retrievedAccount shouldEqual Some(account)
  }
}
