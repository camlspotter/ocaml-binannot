module Module = struct
  type (* Module.t => *) t = Foo (* <= Module.t *) (* (hopefully old) p4 location handling bug *)
end

type t = Zoo of Module.t (* ? Module.t *) 
