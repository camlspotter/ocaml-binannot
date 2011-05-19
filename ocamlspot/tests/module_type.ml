(* SX => *)
module type SX = sig
  type (* SX.t => *) t = int (* <= SX.t *)
end
(* <= SX *)

module type S = sig
  module X : SX (* ? SX *)

  type t = X.t (* ? SX.t *)
end
  
