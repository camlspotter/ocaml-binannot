(* F => *) module F (A : sig end) = struct end (* <= F *) 

(* N => *) module N = struct end (* <= N *)

module M = F (* ? F *) (N (* ? N *))
