# EVM TODO

* EVM executing a bytecode needs access to:
 * Current transaction
 * Current block
 * State MPT (storing accounts). It's already implemented but not populated with data.
 * Contract cache (storing code of accounts). Contract cache is not implemented.
* Other missing pieces:
 * Functionality related to burning gas / calculating available gas etc.
 * Logging and storing logged data in a transaction.
 * Do more research on `f0s: System operations opcodes`.

# Not implemented opcodes.

## 0s: Stop and Arithmetic Operations

#### 0x05    SDIV        Signed integer

Add `DataWord.sdiv(that: DataWord): DataWord` method.

#### 0x06    MOD         Modulo

Add `DataWord.mod(that: DataWord): DataWord` method.

#### 0x07    SMOD        Signed modulo

Add `DataWord.smod(that: DataWord): DataWord` method.

#### 0x08    ADDMOD      Modulo

Add `DataWord.addMod(that: DataWord, modulo: DataWord): DataWord` method.

#### 0x09    MULMOD      Modulo

Add `DataWord.mulMod(that: DataWord, modulo: DataWord): DataWord` method.

#### 0x0b    SIGNEXTEND  Extend length of two's complement signed integer

Add `DataWord.signExtend(that: DataWord, modulo: DataWord): DataWord` method.

## 10s: Comparison & Bitwise Logic Operations

#### 0x11    GT      Greater-than comparison

Already implemented in `DataWord`.

#### 0x12    SLT     Signed less-than comparison

Add `DataWord.slt(that: DataWord, modulo: DataWord): DataWord` method.

#### 0x13    SGT     Signed greater-than comparison

Add `DataWord.sgt(that: DataWord, modulo: DataWord): DataWord` method.

#### 0x17    OR      Bitwise OR operation

Already implemented in `DataWord`.

#### 0x18    XOR     Bitwise XOR operation

Already implemented in `DataWord`.

#### 0x1a    BYTE    Retrieve single byte from word

Add `DataWord.getByte(idx: Int): Byte` method.

## 30s: Environmental Information

#### 0x30    ADDRESS         Get address of currently executing account

Stored in the transaction.

#### 0x31    BALANCE         Get balance of the given account

Fetch account from state MPT and obtain its balance.

#### 0x32    ORIGIN          Get execution origination address

Stored in the transaction.

#### 0x33    CALLER          Get caller address. This is the address of the account that is directly responsible for this execution

Stored in the transaction.

#### 0x36    CALLDATASIZE    Get size of input data in current environment

#### 0x37    CALLDATACOPY    Copy input data in current environment to memory This pertains to the input data passed with the message call instruction or transaction

#### 0x38    CODESIZE        Get size of code running in current environment

#### 0x3a    GASPRICE        Get price of gas in current environment

Stored in the transaction.

#### 0x3b    EXTCODESIZE     Get size of an account's code

We need to implement following steps:
* Fetch account from state MPT
* Get hash of an EVM code
* Fetch a contract from contract cache
* Calculate its size

#### 0x3c    EXTCODECOPY     Copy an account's code to memory

We need to implement following steps:
* Fetch account from state MPT
* Get hash of an EVM code
* Fetch a contract from contract cache
* Copy contract to memory

## 40s: Block Information

#### 0x40    BLOCKHASH   Get the hash of one of the 256 most recent complete blocks

#### 0x41    COINBASE    Get the block's beneficiary address

Stored in the current block.

#### 0x42    TIMESTAMP   Get the block's timestamp

Stored in the current block.

#### 0x43    NUMBER      Get the block's number

Stored in the current block.

#### 0x44    DIFFICULTY  Get the block's difficulty

Stored in the current block.

#### 0x45    GASLIMIT    Get the block's gas limit

Stored in the current block.

## 50s Stack, Memory, Storage and Flow Operations

#### 0x53    MSTORE8     Save byte to memory

Already implemented in `Memory`.

#### 0x58    PC          Get the value of the program counter prior to the increment

#### 0x59    MSIZE       Get the size of active memory in bytes

Already implemented in `Memory`.

#### 0x5a    GAS         Get the amount of available gas, including the corresponding reduction

Whole functionality related to burning gas / calculating available gas etc. need to be implemented.

#### 0x5b    JUMPDEST    Mark a valid destination for jumps

## a0s: Logging Operations

Not implemented.

#### 0xa0    LOG0    Append log record with no topics
#### 0xa1    LOG1    Append log record with one topic
…   …
#### 0xa4    LOG4    Append log record with four topics

## f0s: System operations

#### 0xf0    CREATE          Create a new account with associated code

Creation of accounts is already possible. API for doing that needs to be implemented.

#### 0xf1    CALL            Message-call into an account

Needs more investigation.

#### 0xf2    CALLCODE        Message-call into this account with alternative account's code

Needs more investigation.

#### 0xf3   RETURN  Halt Execution, Mark for deletion

Needs more investigation.

#### 0xf4    DELEGATECALL    Message-call into this account with an alternative account's code, but persisting the current values for `sender` and `value`

Needs more investigation.

#### 0xff    SUICIDE     Halt execution and register account for later deletion

Needs more investigation.
