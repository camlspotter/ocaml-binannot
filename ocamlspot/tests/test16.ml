type u = Test15.w (* ? type w *)

module type T = sig
  type (* type T.t => *) t = Qoo (* <= type T.t *)
  type v = t (* ? type T.t *)
end
