(* M0 => *)
module M0 = struct
end
(* <= M0 *)

module M1 = M0 (* ? M0 *) 

module M2 = M1 (* ? M0 *) (* bug 08/12/10: does not trace all the aliases *)

