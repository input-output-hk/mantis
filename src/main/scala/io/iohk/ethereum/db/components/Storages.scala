package io.iohk.ethereum.db.components

import io.iohk.ethereum.db.storage._

object Storages {

  trait DefaultStorages extends StoragesComp {

    dataSourcesComp: DataSourcesComp =>

    override val storages = new DefaultBlockchainStorages

    class DefaultBlockchainStorages extends Storages {

      def blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      def blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      def blockNumberMappingStorage: BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override def receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override def mptNodeStorage: MptNodeStorage = new MptNodeStorage(dataSources.mptDataSource)

      override def evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeStorage)
    }

  }

  /**
    * As IODB required same length keys, we need a specific storage that pads integer values to be used as keys to match
    * keccak keys. See [[IodbBlockNumberMappingStorage]]
    */
  trait IodbStorages extends StoragesComp {
    dataSourcesComp: DataSourcesComp =>

    override val storages = new DefaultBlockchainStorages

    class DefaultBlockchainStorages extends Storages {

      def blockHeadersStorage: BlockHeadersStorage = new BlockHeadersStorage(dataSources.blockHeadersDataSource)

      def blockBodiesStorage: BlockBodiesStorage = new BlockBodiesStorage(dataSources.blockBodiesDataSource)

      def blockNumberMappingStorage: BlockNumberMappingStorage = new IodbBlockNumberMappingStorage(dataSources.blockHeightsHashesDataSource)

      override def receiptStorage: ReceiptStorage = new ReceiptStorage(dataSources.receiptsDataSource)

      override def mptNodeStorage: MptNodeStorage = new MptNodeStorage(dataSources.mptDataSource)

      override def evmCodeStorage: EvmCodeStorage = new EvmCodeStorage(dataSources.evmCodeStorage)
    }
  }
}
