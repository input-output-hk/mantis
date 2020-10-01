pragma solidity ^0.5.1;

contract Fibonacci {
    uint fib = 0;
    uint prevFib = 0;

    function getStoredFib() public view returns (uint) {
        return fib;
    }

    function getNewFib(uint n) public returns (uint) {
        prevFib = fib;
        fib = calcFib(n);
        return fib;
    }

    function calcFib(uint n) private returns (uint) {
        assert(n >= 0);

        if (n == 0)
            return 0;
        else if (n == 1)
            return 1;
        else
            return calcFib(n - 1) + calcFib(n - 2);
    }
}
