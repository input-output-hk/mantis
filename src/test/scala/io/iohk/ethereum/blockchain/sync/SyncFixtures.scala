package io.iohk.ethereum.blockchain.sync

import java.net.InetSocketAddress

import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.domain.{ Account, BlockHeader }
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{ NewBlock, Status }
import org.bouncycastle.util.encoders.Hex

// scalastyle:off magic.number line.size.limit
trait SyncFixtures {

  val unused = ByteString("unused")
  val totalDifficulty: BigInt = 20

  def mkPeer(number: Int, probe: TestProbe): Peer =
    Peer(new InetSocketAddress(s"127.0.0.$number", 0), probe.ref, incomingConnection = false)

  def mkPeerStatus(number: Int): Status =
  Status(1, 1, totalDifficulty, ByteString(s"peer${number}_bestHash"), unused)

  def mkPeerInfo(status: Status, fork: Boolean = true): PeerInfo =
    PeerInfo(remoteStatus = status, forkAccepted = fork, totalDifficulty = totalDifficulty, maxBlockNumber = 0)

  val EmptyTrieRootHash: ByteString = Account.EmptyStorageRootHash
  val baseBlockHeader = BlockHeader(
    parentHash = unused,
    ommersHash = unused,
    beneficiary = unused,
    stateRoot = EmptyTrieRootHash,
    transactionsRoot = EmptyTrieRootHash,
    receiptsRoot = EmptyTrieRootHash,
    logsBloom = BloomFilter.EmptyBloomFilter,
    difficulty = 0,
    number = 0,
    gasLimit = 0,
    gasUsed = 0,
    unixTimestamp = 0,
    extraData = unused,
    mixHash = unused,
    nonce = unused
  )

  val defaultExpectedTargetBlock = 399500
  val defaultSafeDownloadTarget: Int = defaultExpectedTargetBlock
  val defaultBestBlock: Int = defaultExpectedTargetBlock - 1
  val defaultStateRoot = "deae1dfad5ec8dcef15915811e1f044d2543674fd648f94345231da9fc2646cc"
  val defaultTargetBlock: BlockHeader =
    baseBlockHeader.copy(number = defaultExpectedTargetBlock, stateRoot = ByteString(Hex.decode(defaultStateRoot)))

  val defaultState =
    FastSyncState(targetBlock = defaultTargetBlock, safeDownloadTarget = defaultSafeDownloadTarget, bestBlockHeaderNumber = defaultBestBlock)

  val defaultStateMptLeafWithAccount =
    ByteString(Hex.decode("f86d9e328415c225a782bb339b22acad1c739e42277bc7ef34de3623114997ce78b84cf84a0186cb7d8738d800a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

  def getHeaders(from: BigInt, number: BigInt): Seq[BlockHeader]= {
    val blockHeaders = (from until from + number).map { nr => defaultTargetBlock.copy(number = nr) }

    def genChain(parentHash: ByteString, headers: Seq[BlockHeader], result: Seq[BlockHeader] = Seq.empty): Seq[BlockHeader] = {
      if (headers.isEmpty) {
        result
      } else {
        val newHeader = headers.head.copy(parentHash = parentHash)
        genChain(newHeader.hash, headers.tail, result :+ newHeader)
      }
    }

    val first = blockHeaders.head

    first +: genChain(first.hash, blockHeaders.tail)
  }

  def isNewBlock(msg: Message): Boolean = msg match {
    case _: NewBlock => true
    case _           => false
  }
}
