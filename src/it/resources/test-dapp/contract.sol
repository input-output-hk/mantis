pragma solidity 0.4.8;

contract TestContract {

  function newMessage(uint256 thread, string content) {
    NewMessage(thread, content);
  }

  event NewMessage(uint256 indexed thread, string content);

}
