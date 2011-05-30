(* Target *)

(* Target.E => *) exception E (* <= Target.E *)
let int = 1

external external_value : int -> int = "external_value_impl"
