type (* t => *) t = Foo (* <= t *)

type r = {
  foo : t (* ? t *)
}

module M = struct
  type r' = {
    foo : Type_def.t (* ? t *)
  }
end

