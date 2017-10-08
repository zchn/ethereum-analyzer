pragma solidity ^0.4.8;

// From https://www.blockchain-council.org/solidity/control-structure-solidity-programming-language/

// Solidity follows the same syntax of controle structures as Java script or C.
// hence Most of the control structures from JavaScript are available in Solidity
// except for switch and goto. So there is: if, else, while, do, for, break, continue,
// return, ? :, with the usual semantics known from C or JavaScript.

// Let's quickly go through ther syntax in this lecture.


contract ControlStructure {
  /*
   * @title A Simple Example
   * @author Toshendra Sharma
   * @notice Example for the Solidity Course
   * @dev This is only for demo the simple Coin example
   *
   */
  int public a;

  function ControlStructure(uint input1) {

    // if-else can be used like this
    if(input1 == 2)
      a = 1;
    else
      a = 0;

    // while can be used like this
    while(input1 >= 0){
      if(input1 == 5)
        continue;
      input1 = input1 - 1;
      a++;
    }


    // for loop can be used like this
    for(uint i=0; i<=50; i++)
      {
        a++;
        if(a == 4) break;
      }

    // do while can be used like this
    do{
      a--;
    } while (a>0);

    // Conditional Operator can be used like this
    bool isTrue = (a == 1)? true: false;

  }
}
