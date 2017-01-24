pragma solidity ^0.4.2;

contract Fibonacci {

    function calcFib(uint n) returns (uint) {
        if (n < 0)
            throw;
        else if (n == 0)
            return 0;
        else if (n == 1)
            return 1;
        else
            return calcFib(n - 1) + calcFib(n - 2);
    }

}
