package io.iohk.ethereum.network

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.network.discovery.NodeParser

class NodeParserSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  it should "correctly parse IPv4 nodes" in {
    val testVectors = Table[String, Boolean, Boolean](
      ("nodes", "isValid", "hasCustomUdp"),
      (
        "enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        true,
        false
      ),
      (
        " enode://09c4a2fec7daed400e5e28564e23693b23b2cc5a019b612505631bbe7b9ccf709c1796d2a3d29ef2b045f210caf51e3c4f5b6d3587d43ad5d6397526fa6179@174.112.32.157:30303",
        false,
        false
      ), //Has invalid ' ' character
      (
        "http://6e538e7c1280f0a31ff08b382db5302480f775480b8e68f8febca0ceff81e4b19153c6f8bf60313b93bef2cc34d34e1df41317de0ce613a201d1660a788a03e2@52.206.67.235:30303",
        false,
        false
      ), //Has invalid scheme
      (
        "enode://5fbfb426fbb46f8b8c1bd3dd140f5b511da558cd37d60844b525909ab82e13a25ee722293c829e52cb65c2305b1637fa9a2ea4d6634a224d5f400bfe244ac0de@162-243-55-45:30303",
        false,
        false
      ), //Has invalid IP format
      (
        "enode://a5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        false,
        false
      ), //Has invalid node id size
      (
        "enode://zba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        false,
        false
      ), //Node id has invalid 'z' character
      ("enode://@41.135.121.6:30303", false, false), //Has no node id
      ("enode://41.135.121.6:30303", false, false), //Has no node id
      (
        "enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@:30303",
        false,
        false
      ), //Has no IP
      (
        "enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:",
        false,
        false
      ), //Has no port
      ("", false, false),
      (
        "enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303?discport=30305",
        true,
        true
      ) //custom discovery
    )

    forAll(testVectors) { case (nodeString, valid, hasCustomUdp) =>
      val node = NodeParser.parseNode(nodeString)
      node.isRight shouldEqual valid

      if (valid && !hasCustomUdp)
        node.toOption.get.toUri.toString shouldBe nodeString + "?discport=30303"

      if (valid && hasCustomUdp)
        node.toOption.get.toUri.toString shouldBe nodeString
    }
  }

  it should "correctly parse IPv6 nodes" in {
    val testVectors = Table[String, Option[String]](
      ("nodes", "expectedOutput"),
      (
        "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[ce90:c2c:7000:0:10:0:0:0]:30303",
        Some(
          "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[ce90:c2c:7000:0:10:0:0:0]:30303"
        )
      ), //Has full IPv6 address
      (
        "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[ce90:c2c:7000:0:10::]:30303",
        Some(
          "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[ce90:c2c:7000:0:10:0:0:0]:30303"
        )
      ), //Has partial IPv6 address
      (
        "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[::]:30303",
        Some(
          "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[0:0:0:0:0:0:0:0]:30303"
        )
      ), //Has partial localhost IPv6 address
      (
        "enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[0:0:0]:30303",
        None
      ) //Has short localhost IPv6 address
    )

    forAll(testVectors) { case (nodeString, maybeExpectedOutput) =>
      val node = NodeParser.parseNode(nodeString)
      node.isRight shouldEqual maybeExpectedOutput.nonEmpty
      if (maybeExpectedOutput.nonEmpty)
        node.toOption.get.toUri.toString shouldBe maybeExpectedOutput.get + "?discport=30303"
    }
  }

}
