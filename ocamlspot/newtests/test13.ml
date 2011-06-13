type (* type t => *) t = Foo | Bar of int (* <= type t *)

let _ = Foo (* ? type t *)
let _ = Bar (* ? type t *) 1

type u = A of v (* ? type v *)
and (* type v => *) v = B of u | C (* <= type v *)

let _ = A C (* ? type v *)
let _ = B (* ? type v *) (A C)

type (* type x => *) x = { y : int } (* <= type x *)

let (* x => *) x (* <= x *) = { y = 1 } (* ? type x *)

let _ = function Foo (* ? type t *) -> 1 | Bar (* ? type t *) n -> n

let _ = 
  match { y = 1 } (* ? type x *) with
  | { y = (* n => *) n (* <= n *) } (* ? type x *) -> n (* ? n *)

let _ = x (* ? x *).y (* ? type x *)

let _ = fun ((* fun x => *) x (* <= fun x *) : t (* ? type t *)) -> x (* ? fun x *) 

let _ = (1 : int)

type (* type tt => *) 'a tt = Null | Cons of 'a * 'a (* ? type tt *) tt 
(* <= type tt *)

type 'a uu = Foo of 'a vv (* ? type vv *)
and (* type vv => *) 'a vv = Bar of 'a uu (* <= type vv *)

module M = struct
  type (* type typ => *) typ = F (* <= type typ *)
  let (* value typ => *) typ (* <= value typ *) = 1
end

type mt = M.typ (* ? type typ *)

let _ = (M.F (* ? type typ *) : M.typ (* ? type typ *))
let _ = M.typ (* ? value typ *)
