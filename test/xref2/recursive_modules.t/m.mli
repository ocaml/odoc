module rec A : sig
   type t
   type a = A.t
   type b = B.t
end

and B : sig
   type t
   type a = A.t
   type b = B.t
end

and C : sig
   type t
   type a = A.t
   type b = B.t
end
