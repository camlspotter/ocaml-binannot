(* cvs update -r fixedtypes parsing typing *)

(* recursive types *)
class c = object (self) method m = 1 method s = self end
module type S = sig type t as #c end;;

module M : S = struct type t = c end
module type S' = S with type t = c;;

class d = object inherit c method n = 2 end
module type S2 = S with type t as #d;;
module M2 : S = struct type t = d end;;
module M3 : S = struct type t as #d end;;

module T1 = struct
  type ('a,'b) a = [`A of 'a | `B of 'b]
  type ('a,'b) b = [`Z | ('a,'b) a]
end
module type T2 = sig
  type a and b
  val evala : a -> int
  val evalb : b -> int
end
module type T3 = sig
  type a0 as [> (a0,b0) T1.a]
  and b0 as [> (a0,b0) T1.b]
end
module type T4 = sig
  include T3
  include T2 with type a = a0 and type b = b0
end
module F(X:T4) = struct
  type a = X.a and b = X.b
  let a = X.evala (`B `Z)
  let b = X.evalb (`A(`B `Z))
  let a2b (x : a) : b = `A x
  let b2a (x : b) : a = `B x
end
module M4 = struct
  type a = [`A of a | `B of b | `ZA]
  and b = [`A of a | `B of b | `Z]
  type a0 = a
  type b0 = b
  let rec eval0 = function
      `A a -> evala a
    | `B b -> evalb b
  and evala : a -> int = function
      #T1.a as x -> 1 + eval0 x
    | `ZA -> 3
  and evalb : b -> int = function
      #T1.a as x -> 1 + eval0 x
    | `Z -> 7
end
module M5 = F(M4)
