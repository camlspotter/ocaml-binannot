module M = struct
  let (* x => *) x (* <= x *) = 1
end

let _ = 
  let open M in
  x (* ? x *)
;;
