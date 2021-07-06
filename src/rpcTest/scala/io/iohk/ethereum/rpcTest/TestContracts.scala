package io.iohk.ethereum.rpcTest

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.rpcTest.TestData.firstAccount

object TestContracts {

  //https://github.com/rsksmart/rskj/wiki/Deploying-contracts-using-RPC-calls#publishing-a-contract-using-rpc
  val testContract = "6060604052341561000c57fe5b5b6101598061001c6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063cfae32171461003b575bfe5b341561004357fe5b61004b6100d4565b604051808060200182810382528381815181526020019150805190602001908083836000831461009a575b80518252602083111561009a57602082019150602081019050602083039250610076565b505050905090810190601f1680156100c65780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6100dc610119565b604060405190810160405280600381526020017f486921000000000000000000000000000000000000000000000000000000000081525090505b90565b6020604051908101604052806000815250905600a165627a7a72305820ed71008611bb64338581c5758f96e31ac3b0c57e1d8de028b72f0b8173ff93a10029"

  import io.iohk.ethereum.crypto.kec256

  // https://github.com/ethereum/wiki/wiki/JSON-RPC#example-14
  val storageContract = "0x60606040525b6104d260006000508190555061162e600160005060003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055505b600a8060546000396000f360606040526008565b00"
  val pos0: BigInt = BigInt(1234)
  val mapPos: String = "000000000000000000000000" + firstAccount.address.drop(2) + "0000000000000000000000000000000000000000000000000000000000000001"
  val decoded: Array[Byte] = Hex.decode(mapPos)
  val shaPos: BigInt = BigInt(kec256(decoded))
  val mapResult: BigInt  = BigInt(5678)
  val StorageCodeRuntimeRepresentation = "0x60606040526008565b00"


  /*
    contract Counter {
        uint256 public count = 0;

        event Increment(address indexed who, uint256 indexed newValue);   // declaring event

        function increment(uint256 newValue) public {
            Increment(msg.sender, newValue); // logging event
            count = newValue;
        }
    }
  *
  *
  *
  * */
  val counterEventContract = "0x6060604052600060006000505560d68060186000396000f360606040526000357c01000000000000000000000000000000000000000000000000000000009004806306661abd1460415780637cf5dab014606257603f565b005b604c600480505060cd565b6040518082815260200191505060405180910390f35b607660048080359060200190919050506078565b005b803373ffffffffffffffffffffffffffffffffffffffff167fb182275171042022ff972a26edbd0171bccc74463bd22e56dbbeba4e93b7a66860405180905060405180910390a3806000600050819055505b50565b6000600050548156"
  val readEventContract: String = "06661abd" + "0000000000000000000000000000000000000000000000000000000000000000"
  val incrementEventContract = "7cf5dab0"
  val counterContractEventHash = "0xb182275171042022ff972a26edbd0171bccc74463bd22e56dbbeba4e93b7a668"


  /*
      contract Example1 {
        event Event(uint256 indexed value);

        function emitEvent(uint256 value) public {
            // there is no way to interact with this event
            // in smart contracts
            Event(value);
        }
    }
  * */
  val emitEventContract = "0x606060405260818060106000396000f360606040526000357c0100000000000000000000000000000000000000000000000000000000900480634d43bec9146037576035565b005b604b6004808035906020019091905050604d565b005b807f510e730eb6600b4c67d51768c6996795863364461fee983d92d5e461f209c7cf60405180905060405180910390a25b5056"
  val emitEvent = "4d43bec9"
  val emitEventHash = "0x510e730eb6600b4c67d51768c6996795863364461fee983d92d5e461f209c7cf"

  def writeContract(a: BigInt, funName: String): String = {
    import io.iohk.ethereum.utils.ByteUtils
    val asByteString = ByteString(a.toByteArray)
    funName + Hex.toHexString(ByteUtils.padLeft(asByteString, 32).toArray)
  }

  def createTopic(s :String): String = {
    // 0x + padLeft to 64 bytes
    "0x" + s.reverse.padTo(64, "0").reverse.mkString
  }
}
