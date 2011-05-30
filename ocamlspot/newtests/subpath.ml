module F(M : sig end) = struct
  type (* t => *) t (* <= t *)
end

(* N => *)
module N = struct
end
(* <= N *)

module O = N

type fnt = F(N(* ? N *)).t (* ? t *)
