module M = struct
  let (* M.x => *) x (* <= M.x *) = 1
end

let _ = M.x (* ? M.x *)
