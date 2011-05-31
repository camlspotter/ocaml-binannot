module M = struct
  type (* t => *) t = Foo (* <= t *) 
  let (* v => *) v (* <= v *) = Foo  (* ? t *)
end

module N = struct
  include M
end

let _ = (N.v (* ? v *) : N.t (* ? t *) )



