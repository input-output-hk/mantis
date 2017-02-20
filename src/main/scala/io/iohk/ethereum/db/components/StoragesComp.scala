package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.BlockchainStorages

trait StoragesComp {

  val storages: Storages

  trait Storages extends BlockchainStorages {

    val blockHeadersStorage: BlockHeadersStorage

    val blockBodiesStorage: BlockBodiesStorage

    val blockNumberMappingStorage: BlockNumberMappingStorage

    val receiptStorage: ReceiptStorage

    val mptNodeStorage: MptNodeStorage

    val evmCodeStorage: EvmCodeStorage

  }
}
