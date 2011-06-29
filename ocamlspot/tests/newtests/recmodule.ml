module rec M : sig val f : unit -> unit end = (* M => *) struct

  include N (* ? N *)

  let (* M.f => *) f (* <= M.f *) () = N.g (* ? N.g *) ()

end (* <= M *) and N : sig val g : unit -> unit end = (* N => *) struct

  include M (* ? M *) 

  let (* N.g => *) g (* <= N.g *) () = M.f (* ? M.f *) ()

end (* <= N *)
