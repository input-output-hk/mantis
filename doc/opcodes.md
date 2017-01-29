* Each not implemented opcode needs a code handling it in `OpCode.scala`.
* Program needs access to data of a current block, account, transaction and contract cache.

# Not implemented

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
#### 0x31    BALANCE         Get balance of the given account
#### 0x32    ORIGIN          Get execution origination address
#### 0x33    CALLER          Get caller address. This is the address of the account that is directly responsible for this execution
#### 0x36    CALLDATASIZE    Get size of input data in current environment
#### 0x37    CALLDATACOPY    Copy input data in current environment to memory This pertains to the input data passed with the message call instruction or transaction
#### 0x38    CODESIZE        Get size of code running in current environment
#### 0x3a    GASPRICE        Get price of gas in current environment
#### 0x3b    EXTCODESIZE     Get size of an account's code
#### 0x3c    EXTCODECOPY     Copy an account's code to memory

## 40s: Block Information

#### 0x40    BLOCKHASH   Get the hash of one of the 256 most recent complete blocks
#### 0x41    COINBASE    Get the block's beneficiary address
#### 0x42    TIMESTAMP   Get the block's timestamp
#### 0x43    NUMBER      Get the block's number
#### 0x44    DIFFICULTY  Get the block's difficulty
#### 0x45    GASLIMIT    Get the block's gas limit

## 50s Stack, Memory, Storage and Flow Operations

#### 0x53    MSTORE8     Save byte to memory
#### 0x58    PC          Get the value of the program counter prior to the increment
#### 0x59    MSIZE       Get the size of active memory in bytes
#### 0x5a    GAS         Get the amount of available gas, including the corresponding reduction

## f0s: System operations

#### 0xf0    CREATE          Create a new account with associated code
#### 0xf1    CALL            Message-call into an account
#### 0xf2    CALLCODE        Message-call into this account with alternative account's code
#### 0xf4    DELEGATECALL    Message-call into this account with an alternative account's code, but persisting the current values for `sender` and `value`
#### Halt Execution, Mark for deletion
#### 0xff    SUICIDE     Halt execution and register account for later deletion

# Implemented

0x00    STOP        Halts execution
0x01    ADD         Addition operation
0x02    MUL         Multiplication operation
0x03    SUB         Subtraction operation
0x04    DIV         Integer division operation
0x05    SDIV        Signed integer
0x06    MOD         Modulo
0x07    SMOD        Signed modulo
0x08    ADDMOD      Modulo
0x09    MULMOD      Modulo
0x0a    EXP         Exponential operation

10s: Comparison & Bitwise Logic Operations

0x10    LT      Lesser-than comparison
0x11    GT      Greater-than comparison
0x12    SLT     Signed less-than comparison
0x13    SGT     Signed greater-than comparison
0x14    EQ      Equality  comparison
0x15    ISZERO  Simple not operator
0x16    AND     Bitwise AND operation
0x17    OR      Bitwise OR operation
0x18    XOR     Bitwise XOR operation
0x19    NOT     Bitwise NOT operation
0x1a    BYTE    Retrieve single byte from word
20s: SHA3

0x20    SHA3    Compute Keccak-256 hash
30s: Environmental Information


0x34    CALLVALUE       Get deposited value by the instruction/transaction responsible for this execution
0x35    CALLDATALOAD    Get input data of current environment

0x39    CODECOPY        Copy code running in current environment to memory

40s: Block Information

50s Stack, Memory, Storage and Flow Operations

0x50    POP         Remove item from stack
0x51    MLOAD       Load word from memory
0x52    MSTORE      Save word to memory

0x54    SLOAD       Load word from storage
0x55    SSTORE      Save word to storage
0x56    JUMP        Alter the program counter
0x57    JUMPI       Conditionally alter the program counter

0x5b    JUMPDEST    Mark a valid destination for jumps
60s & 70s: Push Operations

0x60    PUSH1   Place 1 byte item on stack
0x61    PUSH2   Place 2-byte item on stack
…
0x7f    PUSH32  Place 32-byte (full word) item on stack
80s: Duplication Operations

0x80    DUP1    Duplicate 1st stack item
0x81    DUP2    Duplicate 2nd stack item
…
0x8f    DUP16   Duplicate 16th stack item
90s: Exchange Operations

0x90    SWAP1   Exchange 1st and 2nd stack items
0x91    SWAP2   Exchange 1st and 3rd stack items
…   …
0x9f    SWAP16  Exchange 1st and 17th stack items
a0s: Logging Operations

0xa0    LOG0    Append log record with no topics
0xa1    LOG1    Append log record with one topic
…   …
0xa4    LOG4    Append log record with four topics
f0s: System operations

0xf0    CREATE          Create a new account with associated code
0xf1    CALL            Message-call into an account
0xf2    CALLCODE        Message-call into this account with alternative account's code
0xf3    RETURN          Halt execution returning output data
