package io.iohk.ethereum.extvm

import java.nio.ByteOrder
import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Framing
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.Tcp
import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, UInt256}

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxyStorage
import io.iohk.ethereum.utils.{BlockchainConfig, ForkBlockNumbers, MonetaryPolicyConfig, VmConfig}
import io.iohk.ethereum.vm._

object A {
  def main(args: Array[String]): Unit = {
    implicit val as: ActorSystem = ActorSystem("test")
    val connection = Tcp().outgoingConnection("127.0.0.1", 8888)

    val (connOut, connIn) = Source
      .queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(4))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()
    val messageHandler = new MessageHandler(connIn, connOut)
    val externalVmConfig: VmConfig.ExternalConfig = VmConfig.ExternalConfig("mantis", None, "127.0.0.1", 8888)
    val c = new VMClient(externalVmConfig, messageHandler, testMode = false)
    val config = BlockchainConfig(
      forkBlockNumbers = ForkBlockNumbers.Empty.copy(
        homesteadBlockNumber = 1150000,
        eip155BlockNumber = 0,
        difficultyBombPauseBlockNumber = 3000000,
        difficultyBombContinueBlockNumber = 5000000,
        difficultyBombRemovalBlockNumber = 5900000
      ),
      chainId = 0x3d.toByte,
      networkId = 1,
      customGenesisFileOpt = Some("test-genesis.json"),
      customGenesisJsonOpt = None,
      monetaryPolicyConfig =
        MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L, 3000000000000000000L, 2000000000000000000L),
      // unused
      maxCodeSize = None,
      accountStartNonce = UInt256.Zero,
      daoForkConfig = None,
      bootstrapNodes = Set(),
      gasTieBreaker = false,
      ethCompatibleStorage = true,
      treasuryAddress = Address(0)
    )
    c.sendHello("2.0", config)

    messageHandler.awaitMessage[msg.VMQuery]
  }
}

class ExtVMInterface(externaVmConfig: VmConfig.ExternalConfig, blockchainConfig: BlockchainConfig, testMode: Boolean)(
    implicit system: ActorSystem
) extends VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] {
  private var vmClient: Option[VMClient] = None

  initConnection()

  private def initConnection(): Unit = {
    close()

    val connection = Tcp().outgoingConnection(externaVmConfig.host, externaVmConfig.port)

    val (connOut, connIn) = Source
      .queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(4))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()

    val client = new VMClient(externaVmConfig, new MessageHandler(connIn, connOut), testMode)
    client.sendHello(ApiVersionProvider.version, blockchainConfig)
    //TODO: await hello response, check version

    vmClient = Some(client)
  }

  @tailrec
  final override def run(context: PC): PR = {
    if (vmClient.isEmpty) initConnection()

    Try(vmClient.get.run(context)) match {
      case Success(res) => res
      case Failure(ex) =>
        ex.printStackTrace()
        initConnection()
        run(context)
    }
  }

  def close(): Unit = {
    vmClient.foreach(_.close())
    vmClient = None
  }

}
