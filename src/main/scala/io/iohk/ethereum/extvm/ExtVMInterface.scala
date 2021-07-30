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
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.domain.{Account, Address, BlockBody, BlockHeader, LegacyTransaction, SignedTransaction, UInt256}

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxyStorage
import io.iohk.ethereum.utils.{BlockchainConfig, ForkBlockNumbers, MonetaryPolicyConfig, VmConfig}
import io.iohk.ethereum.vm.{
  BlockchainConfigForEvm,
  EvmConfig,
  ProgramContext,
  ProgramResult,
  ProgramState,
  Storage,
  VM,
  WorldStateProxy
}
import org.bouncycastle.util.encoders.Hex

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

    val config = mockConfig
    c.sendHello("2.0", config)
    val pc = programContext(config)

    val res = c.run(pc)
    println(res)
  }

  private def programContext(config: BlockchainConfig) = {

    val bcevm = BlockchainConfigForEvm(config)

    val evmConfig: EvmConfig = EvmConfig.FrontierConfigBuilder(bcevm)

    val senderAddress: Address = Address("0x01")
    val blockHeader = BlockHeader(
      parentHash = ByteString(Hex.decode("8345d132564b3660aa5f27c9415310634b50dbc92579c65a0825d9a255227a71")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
      stateRoot = ByteString(Hex.decode("087f96537eba43885ab563227262580b27fc5e6516db79a6fc4d3bcd241dda67")),
      transactionsRoot = ByteString(Hex.decode("8ae451039a8bf403b899dcd23252d94761ddd23b88c769d9b7996546edc47fac")),
      receiptsRoot = ByteString(Hex.decode("8b472d8d4d39bae6a5570c2a42276ed2d6a56ac51a1a356d5b17c5564d01fd5d")),
      logsBloom = ByteString(Hex.decode("0" * 512)),
      difficulty = BigInt("14005986920576"),
      number = 3125369,
      gasLimit = 4699996,
      gasUsed = 84000,
      unixTimestamp = 1486131165,
      extraData = ByteString(Hex.decode("d5830104098650617269747986312e31332e30826c69")),
      mixHash = ByteString(Hex.decode("be90ac33b3f6d0316e60eef505ff5ec7333c9f3c85c1a36fc2523cd6b75ddb8a")),
      nonce = ByteString(Hex.decode("2b0fb0c002946392"))
    )
    val tx: SignedTransaction = transaction(senderAddress, ByteString(""), 10, 123, 456)
    val emptyWorld: MockWorldState = MockWorldState()

    //    messageHandler.awaitMessage[msg.VMQuery]
    ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, emptyWorld, evmConfig)
  }

  private def mockConfig = BlockchainConfig(
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

  class MockTransaction(
      tx: LegacyTransaction,
      senderAddress: Address,
      pointSign: Byte = 0,
      signatureRandom: BigInt = 0,
      signature: BigInt = 0
  ) extends SignedTransaction(
        tx,
        ECDSASignature(v = pointSign, r = signatureRandom.bigInteger, s = signature.bigInteger)
      )

  val defaultGasPrice: BigInt = 1000

  def transaction(
      senderAddress: Address,
      payload: ByteString,
      value: BigInt,
      gasLimit: BigInt,
      gasPrice: BigInt = defaultGasPrice,
      receivingAddress: Option[Address] = None,
      nonce: BigInt = 0
  ): SignedTransaction =
    new MockTransaction(LegacyTransaction(nonce, gasPrice, gasLimit, receivingAddress, value, payload), senderAddress)
}

object MockWorldState {
  type TestVM = VM[MockWorldState, MockStorage]
  type PS = ProgramState[MockWorldState, MockStorage]
  type PC = ProgramContext[MockWorldState, MockStorage]
  type PR = ProgramResult[MockWorldState, MockStorage]
}

object MockStorage {
  val Empty: MockStorage = MockStorage()

  def fromSeq(words: Seq[UInt256]): MockStorage = {
    val map = words.zipWithIndex.map { case (w, i) => BigInt(i) -> w.toBigInt }.toMap
    MockStorage(map)
  }
}

case class MockStorage(data: Map[BigInt, BigInt] = Map()) extends Storage[MockStorage] {
  def store(offset: BigInt, value: BigInt): MockStorage = {
    val updated =
      if (UInt256(value) == UInt256.Zero)
        data - offset
      else
        data + (offset -> value)

    copy(data = updated)
  }

  def load(addr: BigInt): BigInt =
    data.getOrElse(addr, UInt256.Zero)

  def isEmpty: Boolean =
    data.isEmpty
}

case class MockWorldState(
    accounts: Map[Address, Account] = Map(),
    codeRepo: Map[Address, ByteString] = Map(),
    storages: Map[Address, MockStorage] = Map(),
    numberOfHashes: UInt256 = 0,
    touchedAccounts: Set[Address] = Set.empty,
    noEmptyAccountsCond: Boolean = false
) extends WorldStateProxy[MockWorldState, MockStorage] {

  def getAccount(address: Address): Option[Account] =
    accounts.get(address)

  override def getGuaranteedAccount(address: Address): Account =
    super.getGuaranteedAccount(address)

  def saveAccount(address: Address, account: Account): MockWorldState =
    copy(accounts = accounts + (address -> account))

  def deleteAccount(address: Address): MockWorldState =
    copy(accounts = accounts - address, codeRepo - address, storages - address)

  def getCode(address: Address): ByteString =
    codeRepo.getOrElse(address, ByteString.empty)

  def getStorage(address: Address): MockStorage =
    storages.getOrElse(address, MockStorage.Empty)

  def getBlockHash(number: UInt256): Option[UInt256] =
    if (numberOfHashes >= number && number >= 0)
      Some(UInt256(kec256(number.toString.getBytes)))
    else
      None

  def saveCode(address: Address, code: ByteString): MockWorldState =
    if (code.isEmpty)
      copy(codeRepo = codeRepo - address)
    else
      copy(codeRepo = codeRepo + (address -> code))

  def saveStorage(address: Address, storage: MockStorage): MockWorldState =
    if (storage.isEmpty)
      copy(storages = storages - address)
    else
      copy(storages = storages + (address -> storage))

  def getEmptyAccount: Account = Account.empty()

  override def touchAccounts(addresses: Address*): MockWorldState =
    if (noEmptyAccounts)
      copy(touchedAccounts = touchedAccounts ++ addresses.toSet)
    else
      this

  def clearTouchedAccounts: MockWorldState =
    copy(touchedAccounts = touchedAccounts.empty)

  def noEmptyAccounts: Boolean = noEmptyAccountsCond

  override def keepPrecompileTouched(world: MockWorldState): MockWorldState =
    if (world.touchedAccounts.contains(ripmdContractAddress))
      copy(touchedAccounts = touchedAccounts + ripmdContractAddress)
    else
      this
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
