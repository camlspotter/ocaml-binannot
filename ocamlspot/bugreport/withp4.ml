(* Position of the argument current_id is strange. (So as foreground)

   It is not a bug in ocamlspot nor -annot, probably in camlp4 itself,
   since I found this happens even without any pa_*.cmo linking.  
*)

let t_of_config ~current_id ~foreground config =
  current_id + foreground + config

let t_of_config = fun ~current_id ~foreground config ->
  current_id + foreground + config

let t_of_config = fun ~current_id:current_id ~foreground config ->
  current_id + foreground + config

let f = fun x ~y ~z:z -> x + y + z
let g = (fun ~l -> l), 1
let h = (fun ~(l) -> l), 1
let i = (fun ~a ~b -> a + b), 1

let j = (fun ?l () -> l), 1
let j = (fun ?(l) () -> l), 1
let j = (fun ?l:l () -> l), 1
let j = (fun ?(l=1) () -> l), 1

type t = Foo
type s = t list

type u 
type v = u (* <- points wrong place *) list
 
type ('a,'b) act

module type S = sig
  val x : unit -> ('a, 'b) act (* <- cannot point *) list
end

(*
  to compile, must have lib directory which has required pa_*.cmo's
ocamlc -annot -I +camlp4 -pp 'camlp4o' withp4.ml 
*)
    

