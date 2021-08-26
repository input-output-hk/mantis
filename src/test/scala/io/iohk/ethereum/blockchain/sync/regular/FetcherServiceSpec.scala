package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import cats.data.EitherT

import monix.eval.Task

import org.scalamock.scalatest.MockFactory
import org.scalatest.PrivateMethodTester
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.Mocks.MockValidatorsFailingOnBlockBodies
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.RequestFailed
import io.iohk.ethereum.blockchain.sync.TestSyncConfig
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.domain.TransactionWithAccessList
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockBodies
import io.iohk.ethereum.network.p2p.messages.ETH62.BlockHeaders
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Hex
import akka.stream.scaladsl.SourceQueue

class FetcherServiceSpec
    extends TestKit(ActorSystem("FetcherServiceSpec_System"))
    with AnyFlatSpecLike
    with Matchers
    with PrivateMethodTester
    with MockFactory
    with ScalaFutures
    with NormalPatience
    with TestSyncConfig {

  val genesisHash: ByteString = ByteString(
    Hex.decode("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  )

  //TODO: add a  happy path scenario once the implementation for fetching is finished
  "FetcherService" should "return RequestFailed when asking for a not existing blocks" in {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    (blockchainReader.getBlockHeaderByHash _).expects(*).returning(None).anyNumberOfTimes()
    val sourceQueue = mock[SourceQueue[Block]]
    val fetcherService = new FetcherService(blockchainReader, syncConfig, sourceQueue)
    val peerProbe: TestProbe = TestProbe()
    val peer = Peer(PeerId("peerId"), new InetSocketAddress(9191), peerProbe.ref, true)
    val result = fetcherService.fetchBlocksUntil(peer, Right(ByteString("byteString")), Right(ByteString("byteString")))
    result.value shouldBe Task.now(Left(RequestFailed(peer, "Couldn't find blocks to fetch")))
  }

  //TODO: enable when the implementation for fetching is finished
  it should "return an error when request to fetch headers in fetchBlocks fails" ignore {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    (blockchainReader.getHashByBlockNumber _).expects(*, *).returning(Some(genesisHash))
    val sourceQueue = mock[SourceQueue[Block]]
    val fetcherService = new FetcherService(blockchainReader, syncConfig, sourceQueue)
    val fetchBlocks = PrivateMethod[EitherT[Task, RequestFailed, Peer]]('fetchBlocks)
    val eitherPeerOrError = fetcherService.invokePrivate(fetchBlocks())
    assert(eitherPeerOrError === RequestFailed)
  }

  val emptyTransactionsRoot: ByteString =
    ByteStringUtils.string2hash("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")
  val emptyOmmersHash: ByteString =
    ByteStringUtils.string2hash("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")

  val header1: BlockHeader =
    BlockHeader(
      parentHash = ByteString.empty,
      ommersHash = emptyOmmersHash,
      beneficiary = ByteString.empty,
      stateRoot = ByteString.empty,
      transactionsRoot = emptyTransactionsRoot,
      receiptsRoot = ByteString.empty,
      logsBloom = ByteString.empty,
      difficulty = 0,
      number = 0,
      gasLimit = 0,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString.empty,
      mixHash = ByteString.empty,
      nonce = ByteString.empty
    )

  val header2: BlockHeader = header1.copy(
    ommersHash = // corresponds to unclesNode of header1 so that it matches with body2
      ByteStringUtils.string2hash("cbaa7bde7dbe7062aa8f6c3f3dc772aa07442e5d5561df8b94d5987bd6ebd9fb")
  )

  val body1: BlockBody = BlockBody(Nil, Nil)
  val body2: BlockBody = BlockBody(Nil, Seq(header1))
  val body3: BlockBody = BlockBody(Nil, Seq(header1, header2))

  val peerId: PeerId = PeerId("peerId")

  "FetcherService.BlockIdentifier" should "pair the test headers and bodies" in {
    FetcherService.BlockIdentifier(body1) shouldEqual FetcherService.BlockIdentifier(header1)
    FetcherService.BlockIdentifier(body2) shouldEqual FetcherService.BlockIdentifier(header2)
    (FetcherService.BlockIdentifier(body1) should not).equal(FetcherService.BlockIdentifier(header2))
  }

  "FetcherService.buildBlocks" should "return matching headers and bodies only" in {
    val result = FetcherService.buildBlocks(Seq(header1, header2), Seq(body2, body3, body1))
    result shouldEqual Seq(Block(header1, body1), Block(header2, body2))
  }

  "FetcherService.fetchBlocksForHeaders" should "combine matching headers and bodies" in {
    val messages = Seq(
      MessageFromPeer(BlockHeaders(Seq(header1, header2)), peerId),
      MessageFromPeer(BlockBodies(Seq(body2, body3, body1)), peerId)
    )

    val result = Source(messages).via(FetcherService.fetchBlocksForHeaders(Sink.ignore)).runWith(Sink.seq)

    whenReady(result)(_ shouldEqual Seq(Seq(Block(header1, body1), Block(header2, body2))))
  }

  "FetcherService.fetchBlocksForHeaders" should "divert block request data to given sink" in {
    val messages = Seq(
      MessageFromPeer(BlockHeaders(Seq(header1, header2)), peerId)
    )

    val result = Source(messages)
      .viaMat(FetcherService.fetchBlocksForHeaders(Sink.seq))(Keep.right)
      .toMat(Sink.ignore)(Keep.left)
      .run()

    whenReady(result)(_ shouldEqual Seq(peerId -> Seq(header1.hash, header2.hash)))
  }

  "FetcherService.fetchBlocksForHeaders" should "deal with disjointed request / response cycles" in {
    val messages = Seq(
      MessageFromPeer(BlockHeaders(Seq(header1)), peerId),
      MessageFromPeer(BlockHeaders(Seq(header2)), peerId),
      MessageFromPeer(BlockBodies(Seq(body2, body1)), peerId)
    )

    val result = Source(messages).via(FetcherService.fetchBlocksForHeaders(Sink.ignore)).runWith(Sink.seq)

    whenReady(result)(_ shouldEqual Seq(Seq(Block(header1, body1), Block(header2, body2))))
  }

}
