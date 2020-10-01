pragma solidity ^0.5.1;

contract MutualRecursion {

    function isEven(uint n) public returns (bool) {
        if (n == 0)
            return true;
        else
            return isOdd(n - 1);
    }

    function isOdd(uint n) public returns (bool) {
        if (n == 0)
            return false;
        else
            return isEven(n - 1);
    }
}
