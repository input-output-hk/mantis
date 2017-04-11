package io.iohk.ethereum.evm

import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.evm.util.FixtureProvider
import io.iohk.ethereum.ledger.Ledger
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

import akka.actor.{Actor, ActorRef, _}
import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{BlockHeader, Receipt}
import io.iohk.ethereum.network.PeerActor.{MessageReceived, SendMessage, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer, Peers}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.ReceiptImplicits.receiptRlpEncDec
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.rlp.RLPImplicitConversions.toRlpList
import io.iohk.ethereum.rlp.{RLPEncoder, RLPImplicitConversions, encode}
import org.spongycastle.util.encoders.Hex

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainStorages, Receipt}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBody, BlockHeaderImplicits}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

// scalastyle:off
class ExampleContractTest extends FlatSpec with Matchers {
  "FixtureProvider" should " load data from files" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/evm_test/purchaseContruct/")

    val (storage, stateStorage) = FixtureProvider.prepareStorages(1, fixtures)

    val (st_a, state_after) = FixtureProvider.prepareStorages(2, fixtures)

    //todo remove debug code
    state_after.dataSource.asInstanceOf[EphemDataSource].storage.toSeq.sortBy(_._1.toString)
      .map { case (h, v) =>
        decode[MptNode](v.toArray) match {
          case n: MptLeaf => s"${Hex.toHexString(h.toArray)} => ${n.getAccount}"
          case e => s"${Hex.toHexString(h.toArray)} => $e"
        }
      }
      .foreach(println)
    println(fixtures.receipts.get(fixtures.blockByNumber(2).header.hash))

    println("----------------------------------------")
    println("----------------------------------------")
    println("----------------------------------------")
    println("----------------------------------------")
    //todo remove debug code

    //block containing only ether transfers
    Ledger.executeBlock(fixtures.blockByNumber(2), storage, stateStorage)
  }
}
