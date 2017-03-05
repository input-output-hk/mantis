pragma solidity ^0.4.9;

contract MutualRecursion {

    function isEven(uint n) returns (bool) {
        if (n == 0)
            return true;
        else
            return isOdd(n - 1);
    }

    function isOdd(uint n) returns (bool) {
        if (n == 0)
            return false;
        else
            return isEven(n - 1);
    }
}
