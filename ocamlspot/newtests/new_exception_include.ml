module M = struct
  exception (* E => *) E (* <= E *)
end

module N = struct
  include M
  let _ = raise E (* ? E *)
end

