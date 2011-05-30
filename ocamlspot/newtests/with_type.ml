(* S => *)
module type S = sig
  type (* elt => *) elt (* <= elt *)
end
(* <= S *)

module type T = S (* ? S *) 
  with type elt (* ? elt_impos *) = int 
(* We have no position info for [elt]. No way to query... *)

module M : functor(M' : S (* ? S *)) -> sig type t end with type t = M'.elt
  = functor(M' : (* eltA => *) S (* <= eltA *) ) -> struct 
    (* No position info for the parameter M'. Use the one of S instead *)
    type t = M'.elt (* ? eltA *)
  end

module M2 : functor(M' : (* eltB => *) S (* <= eltB *)) -> sig type t end with type t = M'.elt (* eltB *)
  (* No position info for the parameter M'. Use the one of S instead *)
  = functor(M' : S) -> struct 
    type t = M'.elt
  end

