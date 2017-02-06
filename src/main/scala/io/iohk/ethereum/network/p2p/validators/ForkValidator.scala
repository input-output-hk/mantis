package io.iohk.ethereum.network.p2p.validators

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeader, BlockHeaders}
import org.spongycastle.util.encoders.Hex

/**
  * This Validator checks if a given `BlockHeaders` contains a `BlockHeader` having a specific block number and hash
  *
  * This will be usually used to check if we are dealing with DAO Forked block (number = 1920000 and hash distinct of
  * 94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f)
  *
  * @param blockNumber BlockNumber to check
  * @param blockHash Hash of the block whose number is `blockNumber`
  */
case class ForkValidator(blockNumber: BigInt, blockHash: ByteString) extends MessageValidator[BlockHeaders, ForkValidatorError] {

  /**
    * Validates the header
    *
    * @param message Message to be validate
    * @return an object containing the list of invalid headers found
    */
  override def validate(message: BlockHeaders): Option[ForkValidatorError] = {
    val errors = message.headers.filter { header =>
      header.number == blockNumber && !(header.hash == blockHash)
    }
    errors.headOption.map(_ => ForkValidatorError(errors))
  }
}

case class ForkValidatorError(invalidHeaders: Seq[BlockHeader]) extends MessageValidatorError
