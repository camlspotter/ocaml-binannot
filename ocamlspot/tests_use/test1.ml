let x = 1 (* ../ocamlspot --use test1.ml:v:x__1030 *)

let _ = x

module M = struct
  let y = 1 (* ../ocamlspot --use test1.ml:v:y__1031 *)

  let _ = y
end

let _ = M.y (* M__1032.y__0 *)

module Z = struct (* ../ocamlspot --use test1.ml:m:Z__1034 *)
  let z = 1 (* ../ocamlspot --use test1.ml:v:z__1033 *)
end

