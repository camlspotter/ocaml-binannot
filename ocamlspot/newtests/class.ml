class (* class c => *) c =  (* CR: pos can be improved *)
  let (* x => *) x (* <= x *) = 1 in
  let (* y => *) y (* <= y *) = x (* ? x *) in
  object
    val x = x (* ? x *)
    val y = y (* ? y *)
    val (* vx => *) vx = 1 (* <= vx *) (* CR: pos can be improved *)
    method m = vx (* ? vx *)
end (* <= class c *)

let v = new c (* ? class c *)
;;

