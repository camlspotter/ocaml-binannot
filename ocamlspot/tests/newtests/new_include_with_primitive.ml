(* P => *)
module P = struct
  let chr = 1
  let code = 1
  include Char
  let (* chr => *) chr (* <= chr *) = "hello"
end
(* <= P *)


let _ = P.chr (* ? chr *)
