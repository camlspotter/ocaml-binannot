class (* c0 => *) c0 = object
  val a = 1
  method m = a
end 
(* <= c0 *)

class (* c => *) c ((* p => *) p (* <= p *) : int) = 
  let (* x => *) x (* <= x *) = 1 in
  let p' = p (* ? p *) in
  object (* self => *)(self)(* <= self *)
    (* a => *) inherit c0 (* <= a *)

    (* y => *) val mutable y = x (* <= y *)
    val z = x (* ? x *)
    val p'' = p (* ? p *)
    method f () = x (* ? x *)
    method g () = y (* ? y *)
    method h () = self(* ? self *)#g ()
    method i () = a (* ? a *) (* We cannot follow into c0... *)
    method get_p = p (* ? p *)
    method get_p' = p'
    initializer
      y <- 42
  end
(* <= c *)

let _ = 
  let (* o => *) o (* <= o *) : c (* ? c *) = new c (* ? c *) 42 in
  o(*? o *)#f ()

let o = 
  let (* yy => *) yy (* <= yy *) = 2 in
object 
  (* xx => *) val xx = 1 (* <= xx *) 
  method get_xx = xx (* ? xx *) 
  method get_yy = yy (* ? yy *)
end
