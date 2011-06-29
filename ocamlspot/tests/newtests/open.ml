(* M => *) module M = struct
  let (* x => *) x (* <= x *) = 1
end (* <= M *)

open M (* ? M *)
open Target (* ? Target *)

