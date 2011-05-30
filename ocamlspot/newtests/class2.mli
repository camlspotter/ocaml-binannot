module M0 : sig
  class (* M0.c => *) c : object end (* <= M0.c *)
end
(* <= M0 *)

module Test : sig
  val v : M0.c (* ? M0.c *)
  class type (* ct => *) ct = M0.c (* <= ct *)
  class type ct' = M0.c (* ? M0.c *) 
  val z : ct (* ? ct *)
end
