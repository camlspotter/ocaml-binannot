module type S = (* S => *) sig end (* <= S *)
module M : S (* ? S *) = struct end
