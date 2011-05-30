module O = struct
  module M = struct end
  (* O.M => *) module type M = sig end (* <= O.M *) 
end

module N : O.M (* ? O.M *) = struct end

