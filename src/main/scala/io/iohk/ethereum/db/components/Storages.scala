package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._

object Storages {

  trait DefaultStorages extends StoragesComponent {

    dataSourcesComp: DataSourcesComponent =>

    override val storages: Storages = new DefaultStorages

    class DefaultStorages extends Storages {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override val mptNodeStorage: MptNodeStorage = new MptNodeStorage(dataSources.mptDataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeStorage)
    }

  }

  /**
    * As IODB required same length keys, we need a specific storage that pads integer values to be used as keys to match
    * keccak keys. See [[IodbBlockNumberMappingStorage]]
    */
  trait IodbStorages extends StoragesComponent {
    dataSourcesComp: DataSourcesComponent =>

    override val storages = new DefaultBlockchainStorages

    class DefaultBlockchainStorages extends Storages {

      override val blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      override val blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      override val blockNumberMappingStorage: BlockNumberMappingStorage = new IodbBlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override val receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override val mptNodeStorage: MptNodeStorage = new MptNodeStorage(dataSources.mptDataSource)

      override val evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeStorage)
    }
  }
}
