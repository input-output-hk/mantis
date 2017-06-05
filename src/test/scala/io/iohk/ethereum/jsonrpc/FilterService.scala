package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService.BlockParam

class FilterManager {

  var filters: Seq[Filter]

}

object FilterService {


  case class Filter(id: Long, fromBlock: BlockParam, toBlock: BlockParam, address: ByteString, topics: Seq[ByteString])

}
