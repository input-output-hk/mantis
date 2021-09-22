package io.iohk.ethereum.vm

import akka.util.ByteString

import scala.util.Try

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.crypto.zksnark.BN128.BN128G1
import io.iohk.ethereum.crypto.zksnark.BN128.BN128G2
import io.iohk.ethereum.crypto.zksnark.BN128Fp
import io.iohk.ethereum.crypto.zksnark.PairingCheck
import io.iohk.ethereum.crypto.zksnark.PairingCheck.G1G2Pair
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.ByteStringUtils._
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EtcForks.EtcFork
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks
import io.iohk.ethereum.vm.BlockchainConfigForEvm.EthForks.EthFork

// scalastyle:off magic.number
object PrecompiledContracts {

  val EcDsaRecAddr: Address = Address(1)
  val Sha256Addr: Address = Address(2)
  val Rip160Addr: Address = Address(3)
  val IdAddr: Address = Address(4)
  val ModExpAddr: Address = Address(5)
  val Bn128AddAddr: Address = Address(6)
  val Bn128MulAddr: Address = Address(7)
  val Bn128PairingAddr: Address = Address(8)
  val Blake2bCompressionAddr: Address = Address(9)

  val contracts: Map[Address, PrecompiledContract] = Map(
    EcDsaRecAddr -> EllipticCurveRecovery,
    Sha256Addr -> Sha256,
    Rip160Addr -> Ripemp160,
    IdAddr -> Identity
  )

  val byzantiumAtlantisContracts: Map[Address, PrecompiledContract] = contracts ++ Map(
    ModExpAddr -> ModExp,
    Bn128AddAddr -> Bn128Add,
    Bn128MulAddr -> Bn128Mul,
    Bn128PairingAddr -> Bn128Pairing
  )

  val istanbulPhoenixContracts: Map[Address, PrecompiledContract] = byzantiumAtlantisContracts ++ Map(
    Blake2bCompressionAddr -> Blake2bCompress
  )

  /** Checks whether `ProgramContext#recipientAddr` points to a precompiled contract
    */
  def isDefinedAt(context: ProgramContext[_, _]): Boolean =
    getContract(context).isDefined

  /** Runs a contract for address provided in `ProgramContext#recipientAddr`
    * Will throw an exception if the address does not point to a precompiled contract - callers should first
    * check with `isDefinedAt`
    */
  def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] =
    getContract(context).get.run(context)

  private def getContract(context: ProgramContext[_, _]): Option[PrecompiledContract] =
    context.recipientAddr.flatMap { addr =>
      getContracts(context).get(addr)
    }

  def getContracts(context: ProgramContext[_, _]): Map[Address, PrecompiledContract] = {
    val ethFork = context.evmConfig.blockchainConfig.ethForkForBlockNumber(context.blockHeader.number)
    val etcFork = context.evmConfig.blockchainConfig.etcForkForBlockNumber(context.blockHeader.number)

    if (ethFork >= EthForks.Istanbul || etcFork >= EtcForks.Phoenix) {
      istanbulPhoenixContracts
    } else if (ethFork >= EthForks.Byzantium || etcFork >= EtcForks.Atlantis) {
      // byzantium and atlantis hard fork introduce the same set of precompiled contracts
      byzantiumAtlantisContracts
    } else
      contracts
  }

  sealed trait PrecompiledContract {
    protected def exec(inputData: ByteString): Option[ByteString]
    protected def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt

    def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {

      val ethFork = context.evmConfig.blockchainConfig.ethForkForBlockNumber(context.blockHeader.number)
      val etcFork = context.evmConfig.blockchainConfig.etcForkForBlockNumber(context.blockHeader.number)

      val g = gas(context.inputData, etcFork, ethFork)

      val (result, error, gasRemaining): (ByteString, Option[ProgramError], BigInt) =
        if (g <= context.startGas)
          exec(context.inputData) match {
            case Some(returnData) => (returnData, None, context.startGas - g)
            case None             => (ByteString.empty, Some(PreCompiledContractFail), 0)
          }
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
        error,
        Set.empty,
        Set.empty
      )
    }
  }

  object EllipticCurveRecovery extends PrecompiledContract {
    def exec(inputData: ByteString): Option[ByteString] = {
      val data: ByteString = inputData.padToByteString(128, 0.toByte)
      val h = data.slice(0, 32)
      val v = data.slice(32, 64)
      val r = data.slice(64, 96)
      val s = data.slice(96, 128)

      if (hasOnlyLastByteSet(v)) {
        val recovered = Try(ECDSASignature(r, s, v.last).publicKey(h)).getOrElse(None)
        Some(
          recovered
            .map { bytes =>
              val hash = kec256(bytes).slice(12, 32)
              ByteUtils.padLeft(hash, 32)
            }
            .getOrElse(ByteString.empty)
        )
      } else
        Some(ByteString.empty)

    }

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt = 3000

    private def hasOnlyLastByteSet(v: ByteString): Boolean =
      v.dropWhile(_ == 0).size == 1
  }

  object Sha256 extends PrecompiledContract {
    def exec(inputData: ByteString): Option[ByteString] =
      Some(sha256(inputData))

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt =
      60 + 12 * wordsForBytes(inputData.size)
  }

  object Ripemp160 extends PrecompiledContract {
    def exec(inputData: ByteString): Option[ByteString] =
      Some(ByteUtils.padLeft(ripemd160(inputData), 32))

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt =
      600 + 120 * wordsForBytes(inputData.size)
  }

  object Identity extends PrecompiledContract {
    def exec(inputData: ByteString): Option[ByteString] =
      Some(inputData)

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt =
      15 + 3 * wordsForBytes(inputData.size)
  }

  //Spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-198.md
  object ModExp extends PrecompiledContract {

    private val lengthBytes = 32
    private val totalLengthBytes = 3 * lengthBytes

    def exec(inputData: ByteString): Option[ByteString] = {
      val baseLength = getLength(inputData, 0)
      val expLength = getLength(inputData, 1)
      val modLength = getLength(inputData, 2)

      val result = {
        if (baseLength == 0 && modLength == 0)
          BigInt(0)
        else {
          val mod = getNumber(inputData, safeAdd(totalLengthBytes, safeAdd(baseLength, expLength)), modLength)

          if (mod == 0) {
            BigInt(0)
          } else {
            val base = getNumber(inputData, totalLengthBytes, baseLength)
            val exp = getNumber(inputData, safeAdd(totalLengthBytes, baseLength), expLength)

            base.modPow(exp, mod)
          }
        }
      }
      Some(ByteString(ByteUtils.bigIntegerToBytes(result.bigInteger, modLength)))
    }

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt = {
      val baseLength = getLength(inputData, 0)
      val expLength = getLength(inputData, 1)
      val modLength = getLength(inputData, 2)

      val expBytes =
        inputData.slice(
          safeAdd(totalLengthBytes, baseLength),
          safeAdd(safeAdd(totalLengthBytes, baseLength), expLength)
        )

      if (ethFork >= EthForks.Berlin || etcFork >= EtcForks.Magneto)
        PostEIP2565Cost.calculate(baseLength, modLength, expLength, expBytes)
      else
        PostEIP198Cost.calculate(baseLength, modLength, expLength, expBytes)
    }

    //Spec: https://eips.ethereum.org/EIPS/eip-198
    object PostEIP198Cost {
      private val GQUADDIVISOR = 20

      def calculate(baseLength: Int, modLength: Int, expLength: Int, expBytes: ByteString): BigInt = {
        val multComplexity = getMultComplexity(math.max(baseLength, modLength))
        val adjusted = adjustExpLength(expBytes, expLength)
        multComplexity * math.max(adjusted, 1) / GQUADDIVISOR
      }

      private def getMultComplexity(x: BigInt): BigInt = {
        val x2 = x * x
        if (x <= 64)
          x2
        else if (x <= 1024)
          x2 / 4 + 96 * x - 3072
        else
          x2 / 16 + 480 * x - 199680
      }
    }

    //Spec: https://eips.ethereum.org/EIPS/eip-2565
    object PostEIP2565Cost {
      private val GQUADDIVISOR = 3

      def calculate(baseLength: Int, modLength: Int, expLength: Int, expBytes: ByteString): BigInt = {
        val multComplexity = getMultComplexity(math.max(baseLength, modLength))
        val adjusted = adjustExpLength(expBytes, expLength)
        val r = multComplexity * math.max(adjusted, 1) / GQUADDIVISOR
        if (r <= 200) 200
        else r
      }

      // ceiling(x/8)^2
      private def getMultComplexity(x: BigInt): BigInt =
        ((x + 7) / 8).pow(2)
    }

    private def getNumber(bytes: ByteString, offset: Int, length: Int): BigInt = {
      val number = bytes.slice(offset, safeAdd(offset, length)).padToByteString(length, 0.toByte)
      ByteUtils.toBigInt(number)
    }

    private def safeAdd(a: Int, b: Int): Int =
      safeInt(BigInt(a) + BigInt(b))

    private def safeInt(value: BigInt): Int =
      if (value.isValidInt)
        value.toInt
      else
        Integer.MAX_VALUE

    private def getLength(bytes: ByteString, position: Int): Int = {
      val start = position * lengthBytes
      safeInt(ByteUtils.toBigInt(bytes.slice(start, start + lengthBytes)))
    }

    private def adjustExpLength(expBytes: ByteString, expLength: Int): Long = {
      val expHead =
        if (expLength <= lengthBytes)
          expBytes.padToByteString(expLength, 0.toByte)
        else
          expBytes.take(lengthBytes).padToByteString(lengthBytes, 0.toByte)

      val highestBitIndex = math.max(ByteUtils.toBigInt(expHead).bitLength - 1, 0)

      if (expLength <= lengthBytes) {
        highestBitIndex
      } else {
        8L * (expLength - lengthBytes) + highestBitIndex
      }
    }
  }

  //Spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-196.md
  object Bn128Add extends PrecompiledContract {
    val expectedBytes: Int = 4 * 32

    def exec(inputData: ByteString): Option[ByteString] = {
      val paddedInput = inputData.padToByteString(expectedBytes, 0.toByte)
      val (x1, y1, x2, y2) = getCurvePointsBytes(paddedInput)

      val result = for {
        p1 <- BN128Fp.createPoint(x1, y1)
        p2 <- BN128Fp.createPoint(x2, y2)
        p3 = BN128Fp.toEthNotation(BN128Fp.add(p1, p2))
      } yield p3

      result.map { point =>
        val xBytes = ByteUtils.bigIntegerToBytes(point.x.inner.bigInteger, 32)
        val yBytes = ByteUtils.bigIntegerToBytes(point.y.inner.bigInteger, 32)
        ByteString(xBytes ++ yBytes)
      }
    }

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt =
      if (etcFork >= EtcForks.Phoenix || ethFork >= EthForks.Istanbul)
        BigInt(150) // https://eips.ethereum.org/EIPS/eip-1108
      else
        BigInt(500)

    private def getCurvePointsBytes(input: ByteString): (ByteString, ByteString, ByteString, ByteString) =
      (input.slice(0, 32), input.slice(32, 64), input.slice(64, 96), input.slice(96, 128))

  }

  //Spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-196.md
  object Bn128Mul extends PrecompiledContract {
    val expectedBytes: Int = 3 * 32
    val maxScalar: BigInt = BigInt(2).pow(256) - 1

    def exec(inputData: ByteString): Option[ByteString] = {
      val paddedInput = inputData.padToByteString(expectedBytes, 0.toByte)
      val (x1, y1, scalarBytes) = getCurvePointsBytes(paddedInput)

      val scalar = ByteUtils.toBigInt(scalarBytes)

      val result = for {
        p <- BN128Fp.createPoint(x1, y1)
        s <- if (scalar <= maxScalar) Some(scalar) else None
        p3 = BN128Fp.toEthNotation(BN128Fp.mul(p, s))
      } yield p3

      result.map { point =>
        val xBytes = ByteUtils.bigIntegerToBytes(point.x.inner.bigInteger, 32)
        val yBytes = ByteUtils.bigIntegerToBytes(point.y.inner.bigInteger, 32)
        ByteString(xBytes ++ yBytes)
      }
    }

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt =
      if (etcFork >= EtcForks.Phoenix || ethFork >= EthForks.Istanbul)
        6000 // https://eips.ethereum.org/EIPS/eip-1108
      else
        40000

    private def getCurvePointsBytes(input: ByteString): (ByteString, ByteString, ByteString) =
      (input.slice(0, 32), input.slice(32, 64), input.slice(64, 96))
  }

  //Spec: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-197.md
  // scalastyle: off
  object Bn128Pairing extends PrecompiledContract {
    private val wordLength = 32
    private val inputLength = 6 * wordLength

    val positiveResult: ByteString = ByteUtils.padLeft(ByteString(1), wordLength)
    val negativeResult: ByteString = ByteString(Seq.fill(wordLength)(0.toByte).toArray)

    def exec(inputData: ByteString): Option[ByteString] =
      if (inputData.length % inputLength != 0) {
        None
      } else {
        getPairs(inputData.grouped(inputLength)).map { pairs =>
          if (PairingCheck.pairingCheck(pairs))
            positiveResult
          else
            negativeResult
        }
      }

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt = {
      val k = inputData.length / inputLength
      if (etcFork >= EtcForks.Phoenix || ethFork >= EthForks.Istanbul) { // https://eips.ethereum.org/EIPS/eip-1108
        34000 * k + 45000
      } else {
        80000 * k + 100000
      }
    }

    // Method which stops reading another points if one of earlier ones failed (had invalid coordinates, or was not on
    // BN128 curve
    private def getPairs(bytes: Iterator[ByteString]): Option[Seq[G1G2Pair]] = {
      var accum = List.empty[G1G2Pair]
      while (bytes.hasNext)
        getPair(bytes.next()) match {
          case Some(part) => accum = part :: accum
          case None       => return None // scalastyle:ignore
        }
      Some(accum)
    }

    private def getPair(input: ByteString): Option[G1G2Pair] =
      for {
        g1 <- BN128G1(getBytesOnPosition(input, 0), getBytesOnPosition(input, 1))
        g2 <- BN128G2(
          getBytesOnPosition(input, 3),
          getBytesOnPosition(input, 2),
          getBytesOnPosition(input, 5),
          getBytesOnPosition(input, 4)
        )
      } yield G1G2Pair(g1, g2)

    private def getBytesOnPosition(input: ByteString, pos: Int): ByteString = {
      val from = pos * wordLength
      input.slice(from, from + wordLength)
    }
  }

  //Spec: https://eips.ethereum.org/EIPS/eip-152
  // scalastyle: off
  object Blake2bCompress extends PrecompiledContract {
    def exec(inputData: ByteString): Option[ByteString] =
      Blake2bCompression.blake2bCompress(inputData.toArray).map(ByteString.fromArrayUnsafe)

    def gas(inputData: ByteString, etcFork: EtcFork, ethFork: EthFork): BigInt = {
      val inputArray = inputData.toArray
      if (Blake2bCompression.isValidInput(inputArray)) {
        // Each round costs 1gas
        Blake2bCompression.parseNumberOfRounds(inputArray)
      } else {
        // bad input to contract, contract will not execute, set price to zero
        0
      }
    }
  }
}
