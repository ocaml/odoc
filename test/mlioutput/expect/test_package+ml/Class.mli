(** {0 Module Class}
*)

class type  empty = object
  end

class type  mutually = object
  end

class type  recursive = object
  end

class  mutually' : mutually

class  recursive' : recursive

class type virtual  empty_virtual = object
  end

class virtual  empty_virtual' : empty

class type 'a polymorphic = object
  end

class 'a polymorphic' : 'a polymorphic
