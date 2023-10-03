type node = {
  id   : int;
  up   : node;
  donw : node; 
  left : node;
  right: node;
  col  : int;
  row  : int;
}

type dancelist = {
  head : node;
  cols : node array;
  rows : node array;
}