package io.iohk.ethereum.blockchain.data

import akka.util.ByteString

case class AllocAccount(balance: String)

case class GenesisData(
    nonce: ByteString,
    mixHash: Option[ByteString],
    difficulty: String,
    extraData: ByteString,
    gasLimit: String,
    coinbase: ByteString,
    timestamp: String,
    alloc: Map[String, AllocAccount])
