pragma solidity ^0.4.10;

contract ContractCallingItself {

    uint someVar = 10;

    function callSelf() {
        address selfAddress = this;
        ContractCallingItself selfContract = ContractCallingItself(selfAddress);
        selfContract.doubleSomeVar();
    }

    function doubleSomeVar() {
        someVar = someVar * 2;
    }

    function getSomeVar() constant returns (uint) {
        return someVar;
    }
}
