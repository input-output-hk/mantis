package io.iohk.ethereum.network

import io.iohk.ethereum.network.discovery.NodeParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class NodeParserSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "correctly parse nodes" in {
    val testVectors = Table[String, Boolean](
      ("nodes", "isValid"),

      ("enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        true),
      (" enode://09c4a2fec7daed400e5e28564e23693b23b2cc5a019b612505631bbe7b9ccf709c1796d2a3d29ef2b045f210caf51e3c4f5b6d3587d43ad5d6397526fa6179@174.112.32.157:30303",
        false), //Has invalid ' ' character
      ("http://6e538e7c1280f0a31ff08b382db5302480f775480b8e68f8febca0ceff81e4b19153c6f8bf60313b93bef2cc34d34e1df41317de0ce613a201d1660a788a03e2@52.206.67.235:30303",
        false), //Has invalid scheme
      ("enode://5fbfb426fbb46f8b8c1bd3dd140f5b511da558cd37d60844b525909ab82e13a25ee722293c829e52cb65c2305b1637fa9a2ea4d6634a224d5f400bfe244ac0de@162-243-55-45:30303",
        false), //Has invalid IP format
      ("enode://a5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        false), //Has invalid node id size
      ("enode://c94b6f71c2f3d84ed5587ff936172138cfd4af4951e4ca784b9ea5330f76ed8d77d23a7178b18716947a17a8ef59f18519bc0064e7f3f12e0c1c5934cac147a0@[ce90:c2c:7000:0:10::]:30303",
        false), //Has IPv6 address
      ("enode://zba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:30303",
        false), //Node id has invalid 'z' character
      ("enode://@41.135.121.6:30303",
        false), //Has no node id
      ("enode://41.135.121.6:30303", false), //Has no node id
      ("enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@:30303",
        false), //Has no IP
      ("enode://fba5a07e283d517a2680bcfc7aeb498ac2d246d756556a2ebd5edeb39496491c47a6d27e27f82833b7d7d12defc8de994de04bb58beb72472649f9a323006820@41.135.121.6:",
        false), //Has no port
      ("", false) //Empty node string
    )

    forAll(testVectors) { case (nodeString, valid) =>
        val node = NodeParser.parseNode(nodeString)
        node.isSuccess shouldEqual valid
        if(valid)
          node.get.toUri.toString shouldBe nodeString
    }
  }

}
