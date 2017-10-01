pragma solidity ^0.4.11;

contract owned {
  function owned() { owner = msg.sender; }
  address owner;
}

// Use "is" to derive from another contract. Derived contracts can
// access all members including private functions and storage
// variables.
contract mortal is owned {
  function kill() { if (msg.sender == owner) suicide(owner); }
}
