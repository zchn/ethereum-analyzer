pragma solidity ^0.4.0;

contract C {
  function g() {
    var x = 0;
    (,x) = (1,2,3);
  }
}
