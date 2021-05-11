package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.blocks.{BlockGenerator, TestBlockGenerator}
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.{MockedMinerProtocol, MockedMinerResponse}
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node
import monix.eval.Task

/**
  * Abstraction for a consensus protocol implementation.
  *
  * @see [[io.iohk.ethereum.consensus.Protocol Protocol]]
  */
trait Consensus {

  /**
    * The type of configuration [[io.iohk.ethereum.consensus.FullConsensusConfig#specific specific]]
    * to this consensus protocol implementation.
    */
  type Config <: AnyRef /*Product*/

  def protocol: Protocol

  def config: FullConsensusConfig[Config]

  /**
    * This is the VM used while preparing and generating blocks.
    */
  def vm: VMImpl

  /**
    * Provides the set of validators specific to this consensus protocol.
    */
  def validators: Validators

  /**
    * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
    */
  def blockPreparator: BlockPreparator

  /**
    * Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
    * this consensus protocol uses.
    */
  def blockGenerator: BlockGenerator

  def difficultyCalculator: DifficultyCalculator

  /**
    * Starts the consensus protocol on the current `node`.
    */
  def startProtocol(node: Node): Unit

  /**
    * Stops the consensus protocol on the current node.
    * This is called internally when the node terminates.
    */
  def stopProtocol(): Unit

  /**
    * Sends msg to the internal miner and waits for the response
    */
  def askMiner(msg: MockedMinerProtocol): Task[MockedMinerResponse]

  /**
    * Sends msg to the internal miner
    */
  def sendMiner(msg: MinerProtocol): Unit
}

/**
  * Internal API, used for testing.
  *
  * This is a [[Consensus]] API for the needs of the test suites.
  * It gives a lot of flexibility overriding parts of a consensus' behavior
  * but it is the developer's responsibility to maintain consistency (though the
  * particular consensus protocols we implement so far do their best
  * in that direction).
  */
trait TestConsensus extends Consensus {
  def blockGenerator: TestBlockGenerator

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): TestBlockGenerator

  /** Internal API, used for testing */
  def withValidators(validators: Validators): TestConsensus

  /** Internal API, used for testing */
  def withVM(vm: VMImpl): TestConsensus

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): TestConsensus
}
