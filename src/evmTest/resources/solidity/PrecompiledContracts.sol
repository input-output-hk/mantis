pragma solidity ^0.4.10;

contract PrecompiledContracts {
  function usePrecompiledContracts(bytes32 hash, uint8 v, bytes32 r, bytes32 s) returns (address) {
    return sha256(ripemd160(ecrecover(hash, v, r, s)));
  }
}


