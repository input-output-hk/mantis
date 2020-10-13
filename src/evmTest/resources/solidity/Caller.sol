pragma solidity ^0.5.1;

contract Callee {

  uint foo = 2;

  function setFoo(uint v) public {
    foo = v;
  }

  function getFoo() view public returns (uint) {
      return foo;
  }

}

contract Caller {

  function makeACall(address calleeAddr, uint fooVal) public {
    Callee callee = Callee(calleeAddr);
    callee.setFoo(fooVal);
  }

}
