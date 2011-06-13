class (* c => *) c = object
  (* x => *) val x = 1 (* <= x *)
end (* <= c *)

class nc = object
  inherit let _x = 1 in c (* ? c *)
  val y = 1
  method m = x
end

class nnc = object
  (* nc => *) inherit let _y = 1 in nc (* <= nc *) (* limitation *)
  method n = y (* ? nc *)
end
