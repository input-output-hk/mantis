pragma solidity ^0.5.1;

contract PrecompiledContracts {
  function usePrecompiledContracts(bytes32 hash, uint8 v, bytes32 r, bytes32 s) pure public returns (bytes20) {

    // there is no function alias for Identity contract and we cannot get the return data using low-level call:
    // https://solidity.readthedocs.io/en/latest/types.html#address
    address ecRecovered = ecrecover(hash, v, r, s);
    bytes32 hashed = sha256(abi.encodePacked(ecRecovered));
    bytes20 result = ripemd160(abi.encodePacked(hashed));

    return result;
  }
}


