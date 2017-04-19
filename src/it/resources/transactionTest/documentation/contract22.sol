pragma solidity ^0.4.0;

/*
deploy this contract before homestead and run out of gas -
contract should be created but no code (exceptionalFailedCodeDeposit)

deploy after Homestead - out of gas


redund = new address
deploy normally after before EIP150 and selfdestruct = no charge

deploy normally after EIP150 and selfdestruct = charge for account creation
*/

contract ForksTestContract {

  function selfDestruct(address refundAddr) {
    selfdestruct(refundAddr);
  }

  function exp() returns (uint256) {
    return 4 ** 5;

  }

}
