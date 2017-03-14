package io.iohk.ethereum.network.p2p

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.BlockBroadcastActor.StartBlockBroadcast
import io.iohk.ethereum.network.BlockSendingHandler.SendBlocksToPeers
import io.iohk.ethereum.network.PeerActor.MaxBlockNumber
import io.iohk.ethereum.network.{BlockBroadcastActor, PeerActor}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class BlockSendingHandlerSpec extends FlatSpec with Matchers {

  val NumberPeers = 5

  it should "handle succesful response" in new TestSetup {
    blockBroadcastSetup()

    blockBroadcast ! SendBlocksToPeers(otherPeers.map(_.ref), newBlocks)

    peer.expectNoMsg()
    otherPeers.foreach{ p =>
      p.expectMsgClass(classOf[PeerActor.GetMaxBlockNumber])
      p.reply(PeerActor.MaxBlockNumber(BigInt(0)))

      p.expectMsg(PeerActor.SendMessage(newBlockParent))
      p.expectMsg(PeerActor.SendMessage(newBlock))
    }
  }

  it should "handle succesful response when some of the new blocks should not be send" in new TestSetup {
    blockBroadcastSetup()

    blockBroadcast ! SendBlocksToPeers(otherPeers.map(_.ref), newBlocks)

    peer.expectNoMsg()
    otherPeers.foreach{ p =>
      p.expectMsgClass(classOf[PeerActor.GetMaxBlockNumber])
      p.reply(PeerActor.MaxBlockNumber(BigInt(1)))

      p.expectMsg(PeerActor.SendMessage(newBlock))
      p.expectNoMsg()
    }
  }

  it should "not send any messages if there are no blocks to send in response to RequestMaxBlockNumber" in new TestSetup {
    blockBroadcastSetup()

    blockBroadcast ! SendBlocksToPeers(otherPeers.map(_.ref), Seq())

    peer.expectNoMsg()
    otherPeers.foreach{ p => p.expectNoMsg() }
  }

  it should "not send any messages if there are no blocks to send in response to MaxBlockNumber" in new TestSetup {
    blockBroadcastSetup()

    blockBroadcast ! MaxBlockNumber(BigInt(0))

    peer.expectNoMsg()
    otherPeers.foreach{ p => p.expectNoMsg() }
  }

  trait TestSetup extends BlockUtil with EphemBlockchainTestSetup {
    implicit val system = ActorSystem("MaxBlockRequestHandlerSpec_System")

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

    val peer = TestProbe()
    val otherPeers = (0 to NumberPeers).map(_ => TestProbe())
    val peerManager = TestProbe()

    val blockBroadcast = TestActorRef(BlockBroadcastActor.props(nodeStatusHolder,
      peer.ref,
      peerManager.ref,
      storagesInstance.storages.appStateStorage,
      blockchain))

    val newBlocks = Seq(newBlockParent, newBlock)

    def blockBroadcastSetup(): Unit = {
      blockBroadcast ! StartBlockBroadcast
      peer.expectMsgClass(classOf[PeerActor.Subscribe])
    }
  }

  trait BlockUtil {
    val blockParent = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("3333333333333333333333333333333333333333")),
        stateRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = BigInt("983040"),
        number = 1,
        gasLimit = 134217728,
        gasUsed = 0,
        unixTimestamp = 0,
        extraData = ByteString(Hex.decode("00")),
        mixHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
        nonce = ByteString(Hex.decode("deadbeefdeadbeef"))
      ),
      BlockBody(Seq(), Seq())
    )
    val newBlockParent = NewBlock(blockParent, blockParent.header.difficulty)

    //Son of blockParent
    val block = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("7d7624494a009676b3da30f967c68623e6d940bc53fb8efbbc23369626ef4fac")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("2cad6e80c7c0b58845fcd71ecad6867c3bd4de20")),
        stateRoot = ByteString(Hex.decode("1aa7610066444010b19fb97153d438d7002cc8358b1b96d7687390310acc524b")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
        difficulty = BigInt("982560"),
        number = 2,
        gasLimit = 134086657,
        gasUsed = 0,
        unixTimestamp = 1487334141,
        extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
        mixHash = ByteString(Hex.decode("fe0e6834d52d229fbf4d6f24cb948bab9580e125af359597e25441aef425211e")),
        nonce = ByteString(Hex.decode("5fc20258baa0f466"))
      ),
      BlockBody(Seq(), Seq())
    )
    val newBlock = NewBlock(block, blockParent.header.difficulty + block.header.difficulty)
  }
}
