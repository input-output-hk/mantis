package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.domain.Block._
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xerial.snappy.Snappy

import scala.io.Source

class MessageCompressionSpec extends AnyFlatSpec with Matchers with MockFactory {

  it should "decode block compressed by go" in {
    val testURL = getClass.getResource("/block.go.snappy")
    val res = Source.fromURL(testURL)
    val str = res.getLines().mkString
    val asByteArray = Hex.decode(str)
    val payload = Snappy.uncompress(asByteArray)
    val decoded = payload.toBlock
    decoded.header.hash shouldEqual ByteString(
      Hex.decode("bd64134a158aa767120725614026cc5e614dd67a2cbbcdf72823c97981a08620")
    )
  }
  it should "decode block compressed by python" in {
    val testURL = getClass.getResource("/block.py.snappy")
    val res = Source.fromURL(testURL)
    val str = res.getLines().mkString
    val asByteArray = Hex.decode(str)
    val payload = Snappy.uncompress(asByteArray)
    val decoded = payload.toBlock
    decoded.header.hash shouldEqual ByteString(
      Hex.decode("bd64134a158aa767120725614026cc5e614dd67a2cbbcdf72823c97981a08620")
    )
  }
}
