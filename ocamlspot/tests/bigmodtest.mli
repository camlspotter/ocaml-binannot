(*
        | Psig_value(name, sdesc) ->
        | Psig_type sdecls ->
        | Psig_exception(name, sarg) ->
        | Psig_module(name, smty) ->
        | Psig_recmodule sdecls ->
        | Psig_modtype(name, sinfo) ->
        | Psig_open lid ->
        | Psig_include smty ->
        | Psig_class cl ->
        | Psig_class_type cl ->
*)

(* M0 => *)
module M0 : sig
  val v : int
  type   (* t => *) t (* <= t *) 
  exception E
  module M : sig end
  module rec MR : sig end

  (* MT => *) module type MT = sig 
    type (* s => *) s (* <= s *) 
  end (* <= MT *)

  open Target (* ? Target *)
  include MT (* ? MT *)
  class (* c => *) c : object end (* <= c *)
  class type (* ct => *) ct = object end (* <= ct *)
end
(* <= M0 *)

module Test : sig
  open M0 (* ? M0 *)
  type t = M0.t (* ? t *)
  module M : MT (* ? MT *)
  class c : M0.c (* ? c *)
  class type ct = M0.ct (* ? ct *)
  type s = M0.s (* ? s *) 
end
