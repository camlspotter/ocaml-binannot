module O = struct
  module M = struct end
  module type M = (* O.M => *) sig end (* <= O.M *) 
end

module N : O.M (* ? O.M *) = struct end

