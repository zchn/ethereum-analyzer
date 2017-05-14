// Set interface
library Set {
  
    struct Data { mapping(uint => bool) flags; }

    function insert(Data storage self, uint value) returns (bool);
    function remove(Data storage self, uint value) returns (bool);
    function contains(Data storage self, uint value) returns (bool);
    function version() returns (uint);
}

// SetProvider interface
contract SetProvider { 
    function getSet() returns (Set);
}

contract Bob {
    Set.Data knownValues;

    address public providerAddr;
    
    function Bob(address arg) {
        providerAddr = arg; 
    }

    function getSetVersion() returns (uint) {
        address setAddr = SetProvider(providerAddr).getSet();
        return Set(setAddr).version();  //works only until version 0.3.2
    }
}