module type S = (* S => *) sig end (* <= S *)

(* M => *)
module M : S (* ? S *) = struct end
(* <= M *)

let m = (module M (* ? M *) : S (* ? S *))
module M' = (val m : S (* ? S *))

