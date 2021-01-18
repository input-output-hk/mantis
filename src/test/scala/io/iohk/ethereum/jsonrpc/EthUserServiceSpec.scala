package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString
import io.iohk.ethereum.{NormalPatience, WithActorSystemShutDown, _}
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.EthUserService._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils._
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EthUserServiceSpec
    extends TestKit(ActorSystem("EthServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  it should "handle getCode request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))
    storagesInstance.storages.evmCodeStorage.put(ByteString("code hash"), ByteString("code code code")).commit()

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          Account(0, UInt256(0), ByteString(""), ByteString("code hash"))
        )

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val response = ethUserService.getCode(GetCodeRequest(address, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(GetCodeResponse(ByteString("code code code")))
  }

  it should "handle getBalance request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          Account(0, UInt256(123), ByteString(""), ByteString("code hash"))
        )

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val response = ethUserService.getBalance(GetBalanceRequest(address, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(GetBalanceResponse(123))
  }

  it should "handle MissingNodeException when getting balance" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    val newBlockHeader = blockToRequest.header
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val response = ethUserService.getBalance(GetBalanceRequest(address, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Left(JsonRpcError.NodeNotFound)
  }
  it should "handle getStorageAt request" in new TestSetup {

    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val storageMpt =
      io.iohk.ethereum.domain.EthereumUInt256Mpt
        .storageMpt(
          ByteString(MerklePatriciaTrie.EmptyRootHash),
          storagesInstance.storages.stateStorage.getBackingStorage(0)
        )
        .put(UInt256(333), UInt256(123))

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          Account(0, UInt256(0), ByteString(storageMpt.getRootHash), ByteString(""))
        )

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val response = ethUserService.getStorageAt(GetStorageAtRequest(address, 333, BlockParam.Latest))
    response.runSyncUnsafe().map(v => UInt256(v.value)) shouldEqual Right(UInt256(123))
  }

  it should "handle get transaction count request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(999, UInt256(0), ByteString(""), ByteString("")))

    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val response = ethUserService.getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(GetTransactionCountResponse(BigInt(999)))
  }

  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup {
    override lazy val ledger = mock[Ledger]
    lazy val ethUserService = new EthUserService(
      blockchain,
      ledger,
      blockchainConfig
    )
    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
  }

}
