pragma solidity ^0.4.10;

contract CallSelfDestruct {

  function callDestruct() {
    CallSelfDestruct firstCall = CallSelfDestruct(this);
    firstCall.doSelfdestruct();

    CallSelfDestruct secondCall = CallSelfDestruct(this);
    secondCall.doSelfdestruct();
  }

  function doSelfdestruct() {
    selfdestruct(msg.sender);
  }

}
