pragma solidity ^0.4.0;

contract ForksTestContract {

  uint256 aVar = 0;

  function ForksTestContract() payable {
  }

  function selfDestruct(address refundAddr) {
    selfdestruct(refundAddr);
  }

  function exp() {
    aVar = 4 ** 5;
  }

}
