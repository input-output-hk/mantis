pragma solidity ^0.4.10;

contract Callee {

  uint foo = 2;

  function setFoo(uint v) {
    foo = v;
  }

  function getFoo() constant returns (uint) {
      return foo;
  }

}

contract Caller {

  function makeACall(address calleeAddr, uint fooVal) {
    Callee callee = Callee(calleeAddr);
    callee.setFoo(fooVal);
  }

}
