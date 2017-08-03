package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.ByteUtils

import scala.util.Try

// scalastyle:off magic.number
object PrecompiledContracts {

  val EcDsaRecAddr = Address(1)
  val Sha256Addr = Address(2)
  val Rip160Addr = Address(3)
  val IdAddr = Address(4)

  val contracts = Map(
    EcDsaRecAddr -> EllipticCurveRecovery,
    Sha256Addr -> Sha256,
    Rip160Addr -> Ripemp160,
    IdAddr -> Identity
  )

  /**
    * Given a [[ProgramContext]] it optionally runs a precompiled contract if the receiving address is one of
    * the contracts
    *
    * @return None if the receiving address is not a precompiled contract
    */
  def runOptionally[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): Option[ProgramResult[W, S]] =
    contracts.get(context.receivingAddr).map(_.run(context))

  sealed trait PrecompiledContract {
    protected def exec(inputData: ByteString): ByteString
    protected def gas(inputDataSize: UInt256): BigInt

    def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
      val g = gas(context.env.inputData.size)

      val (result, error, gasRemaining): (ByteString, Option[ProgramError], BigInt) =
        if (g <= context.startGas)
          (exec(context.env.inputData), None, context.startGas - g)
        else
          (ByteString.empty, Some(OutOfGas), 0)

      ProgramResult(
        result,
        gasRemaining,
        context.world,
        Set.empty,
        Nil,
        Nil,
        0,
        error
      )
    }
  }

  object EllipticCurveRecovery extends PrecompiledContract {
    def exec(inputData: ByteString): ByteString = {
      val data = inputData.padTo(128, 0.toByte)
      val h = data.slice(0, 32)
      val v = data.slice(32, 64)
      val r = data.slice(64, 96)
      val s = data.slice(96, 128)

      val recovered = Try(ECDSASignature(r,s,v).publicKey(h)).getOrElse(None)
      recovered.map { bytes =>
        val hash = kec256(bytes).slice(12, 32)
        ByteUtils.padLeft(hash, 32)
      }.getOrElse(ByteString.empty)
    }

    def gas(inputDataSize: UInt256): BigInt =
      3000
  }

  object Sha256 extends PrecompiledContract {
    def exec(inputData: ByteString): ByteString =
      sha256(inputData)

    def gas(inputDataSize: UInt256): BigInt =
      60 + 12 * wordsForBytes(inputDataSize)
  }

  object Ripemp160 extends PrecompiledContract {
    def exec(inputData: ByteString): ByteString =
      ByteUtils.padLeft(ripemd160(inputData), 32)

    def gas(inputDataSize: UInt256): BigInt =
      600 + 120 * wordsForBytes(inputDataSize)
  }

  object Identity extends PrecompiledContract {
    def exec(inputData: ByteString): ByteString =
      inputData

    def gas(inputDataSize: UInt256): BigInt =
      15 + 3 * wordsForBytes(inputDataSize)
  }
}
