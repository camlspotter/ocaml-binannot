type (* constr E => *) t = E (* <= constr E *)

module E = struct let (* module E => *) x (* <= module E *) = 1 end

(* modtype E => *) module type E = sig val x : int end (* <= modtype E *) 

let _ = E (* ? constr E *)

(* exception E => *) exception E (* <= exception E *)

let _ = raise E (* ? exception E *)

let _ = E.x (* ? module E *)

module M : E (* ? modtype E *) = struct
  let x = 1
end
