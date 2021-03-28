(** {0 Module Include}
*)

module type Not_inlined = sig
  
  type t
  end

type t

module type Inlined = sig
  
  type u
  end

type u

module type Not_inlined_and_closed = sig
  
  type v
  end

include Not_inlined_and_closed

module type Not_inlined_and_opened = sig
  
  type w
  end

type w

module type Inherent_Module = sig
  
  val a : t
  end

val a : t

module type Dorminant_Module = sig
  
  val a : t
  
  val a : u
  end

val a : t

val a : u
