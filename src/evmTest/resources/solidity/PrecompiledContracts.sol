pragma solidity ^0.4.10;

contract PrecompiledContracts {
  function usePrecompiledContracts(bytes32 hash, uint8 v, bytes32 r, bytes32 s) returns (bytes20) {

    // there is no function alias for Identity contract and we cannot get the return data using low-level call:
    // https://solidity.readthedocs.io/en/latest/types.html#address

    return ripemd160(sha256(ecrecover(hash, v, r, s)));
  }
}


