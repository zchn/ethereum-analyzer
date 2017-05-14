library Set {
  
    struct Data { mapping(uint => bool) flags; }

    function insert(Data storage self, uint value) public returns (bool) {
        if (self.flags[value])
            return false; // already there
        self.flags[value] = true;
        return true;
    }

    function remove(Data storage self, uint value) public returns (bool) {
        if (!self.flags[value])
            return false; // not there
        self.flags[value] = false;
        return true;
    }

    function contains(Data storage self, uint value) public returns (bool) {
        return self.flags[value];
    }
    
    function version() returns(uint) {
        return 1;
    }
}