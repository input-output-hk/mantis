package io.iohk.ethereum.db.storage

object Namespaces {
  val ReceiptsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('r'.toByte)
  val HeaderNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('h'.toByte)
  val BodyNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('b'.toByte)
  val NodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('n'.toByte)
  val CodeNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('c'.toByte)
  val ChainWeightNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('w'.toByte)
  val AppStateNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('s'.toByte)
  val KnownNodesNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('k'.toByte)
  val HeightsNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('i'.toByte)
  val FastSyncStateNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('f'.toByte)
  val TransactionMappingNamespace: IndexedSeq[Byte] = IndexedSeq[Byte]('l'.toByte)

  val nsSeq = Seq(
    ReceiptsNamespace,
    HeaderNamespace,
    BodyNamespace,
    NodeNamespace,
    CodeNamespace,
    ChainWeightNamespace,
    AppStateNamespace,
    KnownNodesNamespace,
    HeightsNamespace,
    FastSyncStateNamespace,
    TransactionMappingNamespace
  )
}
