package io.iohk.ethereum.consensus.pow

import akka.util.ByteString

sealed trait MinerProtocol

object MinerProtocol {
  case object StartMining extends MinerProtocol
  case object StopMining extends MinerProtocol
}

sealed trait MockedMinerProtocol extends MinerProtocol

object MockedMinerProtocol {
  case class MineBlocks(numBlocks: Int, withTransactions: Boolean, parentBlock: Option[ByteString] = None)
      extends MockedMinerProtocol
}

sealed trait MinerResponse

object MinerResponses {
  case object MinerIsWorking extends MinerResponse
  case object MiningOrdered extends MinerResponse
  case object MinerNotExist extends MinerResponse
  case class MiningError(errorMsg: String) extends MinerResponse
  case class MinerNotSupport(msg: MinerProtocol) extends MinerResponse
}
