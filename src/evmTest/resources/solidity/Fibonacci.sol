pragma solidity ^0.4.10;

contract Fibonacci {
    uint fib = 0;
    uint prevFib = 0;

    function getStoredFib() constant returns (uint) {
        return fib;
    }

    function getNewFib(uint n) returns (uint) {
        prevFib = fib;
        fib = calcFib(n);
        return fib;
    }

    function calcFib(uint n) private returns (uint) {
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
