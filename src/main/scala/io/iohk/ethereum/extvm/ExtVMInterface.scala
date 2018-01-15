package io.iohk.ethereum.extvm

import java.io.{PipedInputStream, PipedOutputStream}

import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm._

class ExtVMInterface(blockchainConfig: BlockchainConfig) extends VM {
  override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    val outboundOut = new PipedOutputStream()
    val outboundIn = new PipedInputStream(outboundOut)

    val inboundOut = new PipedOutputStream()
    val inboundIn = new PipedInputStream(inboundOut)

    new VMServer(blockchainConfig, inboundIn, outboundOut).run()
    val result = new VMClient(context, outboundIn, inboundOut).run()

    outboundIn.close()
    outboundOut.close()
    inboundIn.close()
    inboundOut.close()

    result
  }
}
