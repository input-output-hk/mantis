package io.iohk.ethereum.vm

import akka.util.ByteString

case class ProgramResult(returnData: ByteString, storage: Storage)
