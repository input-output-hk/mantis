package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, UInt256}
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
    * Checks whether `ProgramContext#recipientAddr` points to a precompiled contract
    */
  def isDefinedAt(context: ProgramContext[_, _]): Boolean =
    context.recipientAddr.exists(contracts.isDefinedAt)

  /**
    * Runs a contract for address provided in `ProgramContext#recipientAddr`
    * Will throw an exception if the address does not point to a precompiled contract - callers should first
    * check with `isDefinedAt`
    */
  def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] =
    contracts(context.recipientAddr.get).run(context)


  sealed trait PrecompiledContract {
    protected def exec(inputData: ByteString): ByteString
    protected def gas(inputDataSize: UInt256): BigInt

    def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
      val g = gas(context.inputData.size)

      val (result, error, gasRemaining): (ByteString, Option[ProgramError], BigInt) =
        if (g <= context.startGas)
          (exec(context.inputData), None, context.startGas - g)
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

      if (hasOnlyLastByteSet(v)) {
        val recovered = Try(ECDSASignature(r, s, v.last).publicKey(h)).getOrElse(None)
        recovered.map { bytes =>
          val hash = kec256(bytes).slice(12, 32)
          ByteUtils.padLeft(hash, 32)
        }.getOrElse(ByteString.empty)
      } else
        ByteString.empty

    }

    def gas(inputDataSize: UInt256): BigInt =
      3000

    private def hasOnlyLastByteSet(v: ByteString): Boolean =
      v.dropWhile(_ == 0).size == 1
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
