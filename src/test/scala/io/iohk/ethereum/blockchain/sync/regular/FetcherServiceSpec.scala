package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import cats.data.EitherT

import monix.eval.Task

import org.scalamock.scalatest.MockFactory
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.Mocks.MockValidatorsFailingOnBlockBodies
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.RequestFailed
import io.iohk.ethereum.blockchain.sync.TestSyncConfig
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.utils.Hex

class FetcherServiceSpec
    extends TestKit(ActorSystem("FetcherServiceSpec_System"))
    with AnyFlatSpecLike
    with Matchers
    with PrivateMethodTester
    with MockFactory
    with TestSyncConfig {

  val genesisHash: ByteString = ByteString(
    Hex.decode("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  )

  //TODO: add a  happy path scenario once the implementation for fetching is finished
  "FetcherService" should "return RequestFailed when asking for a not existing blocks" in {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    (blockchainReader.getBlockHeaderByHash _).expects(*).returning(None).anyNumberOfTimes()
    val blockValidator: BlockValidator = mock[BlockValidator]
    val fetcherService = new FetcherService(blockValidator, blockchainReader, syncConfig)
    val peerProbe: TestProbe = TestProbe()
    val peer = Peer(PeerId("peerId"), new InetSocketAddress(9191), peerProbe.ref, true)
    val result = fetcherService.fetchBlocksUntil(peer, Right(ByteString("byteString")), Right(ByteString("byteString")))
    result.value shouldBe Task.now(Left(RequestFailed(peer, "Couldn't find blocks to fetch")))
  }

  //TODO: enable when the implementation for fetching is finished
  it should "return an error when request to fetch headers in fetchBlocks fails" ignore {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    (blockchainReader.getHashByBlockNumber _).expects(*, *).returning(Some(genesisHash))
    val blockValidator: BlockValidator = mock[BlockValidator]
    val fetcherService = new FetcherService(blockValidator, blockchainReader, syncConfig)
    val fetchBlocks = PrivateMethod[EitherT[Task, RequestFailed, Peer]]('fetchBlocks)
    val eitherPeerOrError = fetcherService.invokePrivate(fetchBlocks())
    assert(eitherPeerOrError === RequestFailed)
  }

  it should "fail to verify that bodies are a subset of headers if they don't match" in {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    // Here we are forcing the mismatch between request headers and received bodies
    val blockValidator = new MockValidatorsFailingOnBlockBodies
    val fetcherService = new FetcherService(blockValidator.blockValidator, blockchainReader, syncConfig)
    val bodiesAreOrderedSubsetOfRequested = PrivateMethod[Option[Seq[Block]]]('bodiesAreOrderedSubsetOfRequested)
    val blocks = Seq(Block(ValidBlock.header.copy(number = 97), ValidBlock.body))
    val headers: Seq[BlockHeader] = blocks.map(_.header)
    val bodies: Seq[BlockBody] = blocks.map(_.body)
    val verifiedBlocks: Option[Seq[Block]] =
      fetcherService.invokePrivate(bodiesAreOrderedSubsetOfRequested(headers, bodies, Nil))
    verifiedBlocks shouldBe None
  }

  it should "verify that bodies are a subset of headers" in {
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    val blockValidator = new MockValidatorsAlwaysSucceed
    val fetcherService = new FetcherService(blockValidator.blockValidator, blockchainReader, syncConfig)
    val bodiesAreOrderedSubsetOfRequested = PrivateMethod[Option[Seq[Block]]]('bodiesAreOrderedSubsetOfRequested)
    val blocks = Seq(Block(ValidBlock.header.copy(number = 97), ValidBlock.body))
    val headers: Seq[BlockHeader] = blocks.map(_.header)
    val bodies: Seq[BlockBody] = blocks.map(_.body)
    val verifiedBlocks: Option[Seq[Block]] =
      fetcherService.invokePrivate(bodiesAreOrderedSubsetOfRequested(headers, bodies, Nil))
    verifiedBlocks shouldBe Some(blocks)
  }
}
