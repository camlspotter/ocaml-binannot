(* S => *)
module type S = sig
  type (* elt => *) elt (* <= elt *)
end
(* <= S *)

module X = struct
  type t
end

module type T = S (* ? S *) with type elt (* ? elt_impos *) = X.t

module type F = functor( P : S ) -> S with type elt = P.elt
