module M = struct
  type (* type x => *) x = Foo (* <= type x *)
  let (* x1 => *) x (* <= x1 *) = 1
  let y = x (* ? x1 *)
  let (* x2 => *) x (* <= x2 *) = 2
  let z = x (* ? x2 *)
  let _ = Foo (* ? type x *)
end

let _ = M.x (* ? x2 *)
let _ = M.Foo (* ? type x *)
