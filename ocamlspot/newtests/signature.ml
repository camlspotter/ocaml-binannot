module type T = sig
  module M : sig type (* M.t => *) t = int (* <= M.t *) end
  val f : M.t (* ? M.t *)
end

module MT = struct
  module MM = struct type t = int end
  let f : MM.t = 1
end
  
