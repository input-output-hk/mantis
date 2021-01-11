package io.iohk.ethereum.extvm

import akka.util.ByteString
import scalapb.GeneratedMessageCompanion
import io.iohk.ethereum.domain.{Account, Address, UInt256}
import org.scalamock.scalatest.MockFactory
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorldSpec extends AnyFlatSpec with Matchers with MockFactory {

  import Implicits._

  "World" should "request and cache code" in new TestSetup {
    val code = ByteString(Hex.decode("1122334455FFCC"))

    val expectedCodeQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetCode(msg.GetCode(addr)))
    (messageHandler.sendMessage _).expects(expectedCodeQueryMsg).once()
    (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.Code])).expects(*).returns(msg.Code(code)).once()

    world.getCode(addr) shouldBe code
    world.getCode(addr) shouldBe code
  }

  it should "request and cache account" in new TestSetup {
    val account = Account(0, 123)

    val expectedAccountQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetAccount(msg.GetAccount(addr)))
    (messageHandler.sendMessage _).expects(expectedAccountQueryMsg).once()
    (messageHandler
      .awaitMessage(_: GeneratedMessageCompanion[msg.Account]))
      .expects(*)
      .returns(msg.Account(account.nonce, account.balance, true))
      .once()

    world.getAccount(addr) shouldBe Some(account)
    world.getAccount(addr) shouldBe Some(account)
  }

  it should "request and cache blockhash" in new TestSetup {
    val offset = 10
    val blockhash = UInt256(123123123)

    val expectedBlockchashQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetBlockhash(msg.GetBlockhash(offset)))
    (messageHandler.sendMessage _).expects(expectedBlockchashQueryMsg).once()
    (messageHandler
      .awaitMessage(_: GeneratedMessageCompanion[msg.Blockhash]))
      .expects(*)
      .returns(msg.Blockhash(blockhash))
      .once()

    world.getBlockHash(offset) shouldBe Some(blockhash)
    world.getBlockHash(offset) shouldBe Some(blockhash)
  }

  it should "request and cache storage data" in new TestSetup {
    val offset = UInt256(1024)
    val storageData = UInt256(901919239123L)

    val expectedStorageDataQueryMsg =
      msg.VMQuery(query = msg.VMQuery.Query.GetStorageData(msg.GetStorageData(addr, offset)))
    (messageHandler.sendMessage _).expects(expectedStorageDataQueryMsg).once()
    (messageHandler
      .awaitMessage(_: GeneratedMessageCompanion[msg.StorageData]))
      .expects(*)
      .returns(msg.StorageData(storageData))
      .once()

    world.getStorage(addr).load(offset) shouldBe storageData.toBigInt
    world.getStorage(addr).load(offset) shouldBe storageData.toBigInt
  }

  trait TestSetup {
    val addr = Address("0xFF")
    val messageHandler = mock[MessageHandler]
    val world = World(accountStartNonce = 0, noEmptyAccountsCond = true, messageHandler = messageHandler)
  }

}
