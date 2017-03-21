package io.iohk.ethereum.blockchain.data

import akka.util.ByteString

case class AllocAccount(balance: String)

case class GenesisData(
    nonce: ByteString,
    mixHash: ByteString,
    difficulty: String,
    extraData: ByteString,
    gasLimit: String,
    coinbase: ByteString,
    timestamp: Long,
    alloc: Map[String, AllocAccount])
