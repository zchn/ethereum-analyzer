pragma solidity ^0.4.0;

contract C {
  uint[] data;

  function f() returns (uint, bool, uint) {
    return (7, true, 2);
  }

  function g() {
    // Declares and assigns the variables. Specifying the type explicitly is not possible.
    var (x, b, y) = f();
    // Assigns to a pre-existing variable.
    (x, y) = (2, 7);
    // Common trick to swap values -- does not work for non-value storage types.
    (x, y) = (y, x);
    // Components can be left out (also for variable declarations).
    // If the tuple ends in an empty component,
    // the rest of the values are discarded.
    (data.length,) = f(); // Sets the length to 7
    // The same can be done on the left side.
    (,data[3]) = f(); // Sets data[3] to 2
    // Components can only be left out at the left-hand-side of assignments, with
    // one exception:
    (x,) = (1,);
    // (1,) is the only way to specify a 1-component tuple, because (1) is
    // equivalent to 1.
  }
}
