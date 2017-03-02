package io.iohk.ethereum.vm

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Generators._
import akka.util.ByteString
import org.scalacheck.Gen

class ProgramSpec extends FlatSpec with Matchers with PropertyChecks {

  val MaxCodeSize = Byte.MaxValue
  val MaxJumpDestPositions = 10

  val nonPushOp: Byte = JUMP.code
  val invalidOpCode: Byte = 0xef.toByte

  val positionsListGen: Gen[List[Int]] = getListGen(minSize = 0, maxSize = MaxJumpDestPositions, genT = intGen)

  "Program" should "detect all jump destinations if there are no push op" in {
    forAll(positionsListGen) { jumpDestLocations =>
      val code = ByteString((0 to MaxCodeSize).map{i =>
        if(jumpDestLocations.contains(i)) JUMPDEST.code
        else nonPushOp
      }.toArray)
      val program = Program(code)
      (program.validJumpDestinations diff jumpDestLocations.distinct) shouldBe Seq()
    }
  }

  it should "detect all jump destinations if there are push op" in {
    forAll(positionsListGen, positionsListGen) { (jumpDestLocations, pushOpLocations) =>
      val code = ByteString((0 to MaxCodeSize).map{i =>
        if(jumpDestLocations.contains(i)) JUMPDEST.code
        else if (pushOpLocations.contains(i)) PUSH1.code
        else nonPushOp
      }.toArray)
      val program = Program(code)
      val jumpDestLocationsWithoutPushBefore = jumpDestLocations.distinct.filter(i => pushOpLocations.contains(i - 1))
      (program.validJumpDestinations diff jumpDestLocationsWithoutPushBefore) shouldBe Seq()
    }
  }

  it should "detect all jump destinations if there are invalid ops" in {
    forAll(positionsListGen, positionsListGen) { (jumpDestLocations, invalidOpLocations) =>
      val code = ByteString((0 to MaxCodeSize).map{i =>
        if(jumpDestLocations.contains(i)) JUMPDEST.code
        else if (invalidOpLocations.contains(i)) invalidOpCode
        else nonPushOp
      }.toArray)
      val program = Program(code)
      (program.validJumpDestinations diff jumpDestLocations.distinct) shouldBe Seq()
    }
  }

  it should "detect all instructions as jump destinations if they are" in {
    val code = ByteString((0 to MaxCodeSize).map(_ => JUMPDEST.code).toArray)
    val program = Program(code)
    (program.validJumpDestinations diff (0 to MaxCodeSize)) shouldBe Seq()
  }
}
