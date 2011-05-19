let z = 1

type (* constr E => *) t = E (* <= constr E *)

(* module E => *) module E = struct let (* E.x => *) x (* <= E.x *) = 1 end (* <= module E *) 

(* modtype E => *) module type E = sig val x : int end (* <= modtype E *) 

let _ = E (* ? constr E *)

(* exception E => *) exception E (* <= exception E *)

let _ = raise E (* ? exception E *)

let _ = E.x (* ? E.x *)

(* module M => *)
module M : E (* ? modtype E *) = struct
  let x = 1
end
(* <= module M *)
