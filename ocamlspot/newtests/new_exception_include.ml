module M = struct
  (* E => *) exception E (* <= E *)
end

module N = struct
  include M
  let _ = raise E (* ? E *)
end

