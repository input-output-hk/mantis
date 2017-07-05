package io.iohk.ethereum.mining.pow

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import org.spongycastle.util.encoders.Hex

object PowCache {

  val JEpoch = 30000

  def seedForBlock(blockNumber: BigInt): ByteString =
    (BigInt(0) until (blockNumber / JEpoch)).foldLeft(ByteString(Hex.decode("00" * 32))) { case (b, _) => kec256(b) }

}
