package io.iohk.ethereum.consensus.mining

import monix.eval.Task

import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerProtocol
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponse
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder.Node

/** Abstraction for a mining protocol implementation.
  *
  * @see [[Protocol Protocol]]
  */
trait Mining {

  /** The type of configuration [[FullMiningConfig#specific specific]]
    * to this consensus protocol implementation.
    */
  type Config <: AnyRef /*Product*/

  def protocol: Protocol

  def config: FullMiningConfig[Config]

  /** This is the VM used while preparing and generating blocks.
    */
  def vm: VMImpl

  /** Provides the set of validators specific to this consensus protocol.
    */
  def validators: Validators

  /** This is used by the [[Mining#blockGenerator blockGenerator]].
    */
  def blockPreparator: BlockPreparator

  /** Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
    * this consensus protocol uses.
    */
  def blockGenerator: BlockGenerator

  def difficultyCalculator: DifficultyCalculator

  /** Starts the consensus protocol on the current `node`.
    */
  def startProtocol(node: Node): Unit

  /** Stops the consensus protocol on the current node.
    * This is called internally when the node terminates.
    */
  def stopProtocol(): Unit

  /** Sends msg to the internal miner and waits for the response
    */
  def askMiner(msg: MockedMinerProtocol): Task[MockedMinerResponse]

  /** Sends msg to the internal miner
    */
  def sendMiner(msg: MinerProtocol): Unit
}

/** Internal API, used for testing.
  *
  * This is a [[Mining]] API for the needs of the test suites.
  * It gives a lot of flexibility overriding parts of a consensus' behavior
  * but it is the developer's responsibility to maintain consistency (though the
  * particular consensus protocols we implement so far do their best
  * in that direction).
  */
trait TestMining extends Mining {
  def blockGenerator: TestBlockGenerator

  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): TestBlockGenerator

  /** Internal API, used for testing */
  def withValidators(validators: Validators): TestMining

  /** Internal API, used for testing */
  def withVM(vm: VMImpl): TestMining

  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): TestMining
}
