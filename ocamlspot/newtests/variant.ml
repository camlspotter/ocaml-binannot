module M = struct
  type (* M.t => *) t = Foo of int (* <= M.t *)
end

let _ = M.Foo (* ? M.t *) 1
