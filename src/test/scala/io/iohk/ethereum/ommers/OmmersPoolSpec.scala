package io.iohk.ethereum.ommers

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import io.iohk.ethereum.Fixtures.Blocks.Block3125369
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainReader}
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, GetOmmers}
import io.iohk.ethereum.WithActorSystemShutDown
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class OmmersPoolSpec
    extends TestKit(ActorSystem("OmmersPoolSpec_System"))
    with AnyFreeSpecLike
    with ImplicitSender
    with WithActorSystemShutDown
    with Matchers
    with MockFactory {

  "OmmersPool" - {

    "should not return ommers if there is no any" in new TestSetup {

      /**
        *   00 --> 11 --> 21 --> [31]  (chain1)
        *      \-> 14                  (chain4)
        *  [] new block, reference!
        *  () ommer given the new block
        */
      (blockchainReader.getBlockHeaderByHash _).expects(block2Chain1.hash).returns(Some(block2Chain1))
      (blockchainReader.getBlockHeaderByHash _).expects(block1Chain1.hash).returns(Some(block1Chain1))
      (blockchainReader.getBlockHeaderByHash _).expects(block0.hash).returns(Some(block0))

      ommersPool ! AddOmmers(
        block0,
        block1Chain1,
        block1Chain4,
        block2Chain1
      )

      ommersPool ! GetOmmers(block3Chain1.parentHash)
      expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq.empty))
    }

    "should return ommers properly" - {

      "in case of a chain with less length than the generation limit" in new TestSetup {

        /**
          *   00 --> (11) --> 21 --> 31  (chain1)
          *    \        \        \-> 33  (chain3)
          *     \        \--> 22 --> 32  (chain2)
          *      \-> [14]                (chain4)
          *  [] new block, reference!
          *  () ommer given the new block
          */
        (blockchainReader.getBlockHeaderByHash _).expects(block0.hash).returns(Some(block0))
        (blockchainReader.getBlockHeaderByHash _).expects(block0.parentHash).returns(None)

        ommersPool ! AddOmmers(
          block0,
          block1Chain1,
          block2Chain1,
          block2Chain2,
          block3Chain1,
          block3Chain2,
          block3Chain3
        )

        ommersPool ! GetOmmers(block1Chain4.parentHash)
        expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(block1Chain1)))
      }

      "despite of start losing older ommers candidates" in new TestSetup {

        /**
          *   XX -->  (11) -->  21 --> 31  (chain1)
          *    \         \         \-> 33  (chain3)
          *     \         \-->  22 --> 32  (chain2)
          *      \-->  14 ---> [24]        (chain4)
          *       \-> (15)                 (chain5)
          *  [] new block, reference!
          *  () ommer given the new block
          *  XX removed block
          */
        (blockchainReader.getBlockHeaderByHash _).expects(block1Chain4.hash).returns(Some(block1Chain4)).once()
        (blockchainReader.getBlockHeaderByHash _).expects(block0.hash).returns(Some(block0)).once()
        (blockchainReader.getBlockHeaderByHash _).expects(block0.parentHash).returns(None).once()

        ommersPool ! AddOmmers(
          block0,
          block1Chain1,
          block2Chain1,
          block3Chain1,
          block1Chain4,
          block2Chain2,
          block3Chain2,
          block3Chain3
        )

        // Ommers pool size limit is reach, block0 will be removed.
        // Notice that in terms of additions, current pool implementation is behaving as a queue with a fixed size!
        ommersPool ! AddOmmers(block1Chain5)

        ommersPool ! GetOmmers(block2Chain4.parentHash)
        expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(block1Chain5, block1Chain1)))
      }

      "by respecting size and generation limits" in new TestSetup {

        /**
          *   00 --> 11 -->  21  --> [31]  (chain1)
          *    \      \          \-> (33)  (chain3)
          *     \      \--> (22) -->  32   (chain2)
          *      \-> 14                    (chain4)
          *  [] new block, reference!
          *  () ommer given the new block
          */
        (blockchainReader.getBlockHeaderByHash _).expects(block2Chain1.hash).returns(Some(block2Chain1))
        (blockchainReader.getBlockHeaderByHash _).expects(block1Chain1.hash).returns(Some(block1Chain1))
        (blockchainReader.getBlockHeaderByHash _).expects(block0.hash).returns(Some(block0))

        ommersPool ! AddOmmers(
          block0,
          block1Chain1,
          block2Chain1,
          block1Chain4,
          block2Chain2,
          block3Chain2,
          block3Chain3
        )

        ommersPool ! GetOmmers(block3Chain1.parentHash)
        expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(block2Chain2, block3Chain3)))
      }

    }
  }

  trait TestSetup extends MockFactory {

    // In order to support all the blocks for the given scenarios
    val ommersPoolSize: Int = 8

    // Originally it should be 6 as is stated on section 11.1, eq. (143) of the YP
    // Here we are using a simplification for testing purposes
    val ommerGenerationLimit: Int = 2
    val returnedOmmerSizeLimit: Int = 2 // Max amount of ommers allowed per block

    /**
      *   00 ---> 11 --> 21 --> 31 (chain1)
      *    \       \       \--> 33 (chain3)
      *     \       \--> 22 --> 32 (chain2)
      *      \--> 14 --> 24        (chain4)
      *       \-> 15               (chain5)
      */
    val block0 = Block3125369.header.copy(number = 0, difficulty = 0)

    val block1Chain1 = Block3125369.header.copy(number = 1, parentHash = block0.hash, difficulty = 11)
    val block2Chain1 = Block3125369.header.copy(number = 2, parentHash = block1Chain1.hash, difficulty = 21)
    val block3Chain1 = Block3125369.header.copy(number = 3, parentHash = block2Chain1.hash, difficulty = 31)

    val block2Chain2 = Block3125369.header.copy(number = 2, parentHash = block1Chain1.hash, difficulty = 22)
    val block3Chain2 = Block3125369.header.copy(number = 2, parentHash = block2Chain2.hash, difficulty = 32)

    val block3Chain3 = Block3125369.header.copy(number = 3, parentHash = block2Chain1.hash, difficulty = 33)

    val block1Chain4 = Block3125369.header.copy(number = 1, parentHash = block0.hash, difficulty = 14)
    val block2Chain4 = Block3125369.header.copy(number = 2, parentHash = block1Chain4.hash, difficulty = 24)

    val block1Chain5 = Block3125369.header.copy(number = 1, parentHash = block0.hash, difficulty = 15)

    val testProbe = TestProbe()

    val blockchainReader = mock[BlockchainReader]
    val ommersPool =
      system.actorOf(
        OmmersPool.props(blockchainReader, ommersPoolSize, ommerGenerationLimit, returnedOmmerSizeLimit)
      )
  }
}
