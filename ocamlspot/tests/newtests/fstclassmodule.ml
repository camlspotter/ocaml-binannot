module type S = (* S => *) sig
  type t
  val x : t list
end (* <= S *)

(* M => *)
module M : S (* ? S *) with type t = int = struct
  type t = int
  let x = [1]
end
(* <= M *)

let (* m => *) m (* <= m *) = (module M (* ? M *) : S (* ? S *) with type t = int)
module M' = (val m (* ? m *) : S (* ? S *) with type t = int)
