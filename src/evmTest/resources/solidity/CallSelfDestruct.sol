pragma solidity ^0.5.1;

contract CallSelfDestruct {

  function callDestruct() public {
    CallSelfDestruct firstCall = CallSelfDestruct(this);
    firstCall.doSelfdestruct();

    CallSelfDestruct secondCall = CallSelfDestruct(this);
    secondCall.doSelfdestruct();
  }

  function doSelfdestruct() public {
    selfdestruct(msg.sender);
  }

}
