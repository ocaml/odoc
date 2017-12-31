type point = {
  line : int;
  column : int;
}

type span = {
  start : point;
  end_ : point;
}

type 'a with_location = {
  location : span;
  value : 'a;
}
