package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalacheck.Gen

object Generators extends ObjectGenerators {
  val testStackMaxSize = 32

  def dataWordGen: Gen[DataWord] = bigIntGen.map(DataWord(_))

  def stackGen(maxSize: Int = testStackMaxSize): Gen[Stack] = {
    val sizeGen = Gen.choose(0, maxSize)
    val listGen = sizeGen.flatMap(Gen.listOfN(_, dataWordGen))
    val stack = Stack(maxSize)
    listGen.flatMap(stack.push(_).right.get)
  }

  case class ProgramStateGenBuilder(stackGen: Gen[Stack] = stackGen()) {
    def gen: Gen[ProgramState] = {
      val invoke = ProgramInvoke(new Program(ByteString.empty), ByteString.empty, ByteString.empty, Storage())
      for {
        stack <- stackGen
      } yield ProgramState(invoke).withStack(stack)
    }

    def withStack(stackGen: Gen[Stack]): ProgramStateGenBuilder = copy(stackGen = stackGen)
  }
}
