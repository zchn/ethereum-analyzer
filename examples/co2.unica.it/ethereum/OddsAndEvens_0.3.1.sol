contract OddsAndEvens{

  struct Player {
    address addr;
    uint number;
  }
  
  Player[2] public players;         //public only for debug purpose

  uint8 tot;
  address owner;

  function OddsAndEvens() {
    owner = msg.sender;
  }
  
  function play(uint number) {
    if (msg.value != 1 ether) throw;
    
    players[tot] = Player(msg.sender, number);
    tot++;
    
    if (tot==2) andTheWinnerIs();
  }
  
  function andTheWinnerIs() private {
    uint n = players[0].number+players[1].number;
    if (n%2==0) {
      players[0].addr.send(1800 finney);
    }
    else {
      players[1].addr.send(1800 finney);
    }
        
    delete players;
    tot=0;
  }
  
  function getProfit() {
    if(msg.sender!=owner) throw;
    msg.sender.send(this.balance);
  }

}
