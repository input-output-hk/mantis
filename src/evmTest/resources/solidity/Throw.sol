pragma solidity ^0.5.1;

contract Throw {
    function justThrow() pure public {
        assert(false);
    }
}
