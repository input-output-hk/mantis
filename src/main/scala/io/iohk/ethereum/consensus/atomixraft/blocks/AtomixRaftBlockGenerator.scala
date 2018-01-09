package io.iohk.ethereum.consensus.atomixraft.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.utils.BlockchainConfig

class AtomixRaftBlockGenerator(
  blockchain: Blockchain,
  blockchainConfig: BlockchainConfig,
  consensusConfig: ConsensusConfig,
  blockPreparator: BlockPreparator,
  blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends NoOmmersBlockGenerator(
  blockchain,
  blockchainConfig,
  consensusConfig,
  blockPreparator,
  blockTimestampProvider
) {

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): AtomixRaftBlockGenerator =
    new AtomixRaftBlockGenerator(
      blockchain,
      blockchainConfig,
      consensusConfig,
      blockPreparator,
      blockTimestampProvider
    )
}

