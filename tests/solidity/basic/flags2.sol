contract Test {
  event Flag(bool);

  bool private flag0 = true;
  bool private flag1 = true;

  function set0(int val) public returns (bool){
    return(false);
  }

  function set1(int val) public returns (bool){
    return(true);
  }

  function echidna_alwaystrue() public returns (bool){
    return(true);
  }

  function echidna_revert_always() public returns (bool){
    revert();
  }

  function echidna_sometimesfalse() public returns (bool){
    emit Flag(flag0);
    emit Flag(flag1);
    return(flag1);
  }

}
