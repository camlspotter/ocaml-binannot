type (* record => *) record = { mutable x : int } (* <= record *)

let (* r => *) r (* <= r *) = { x = 0; } (* ? record *)

let _ = r(* ? r *).x <- (* ? record *) 1 

 
