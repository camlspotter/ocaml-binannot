let _ =
  let module G = 
    (* module G => *)
    struct let x = 1 end
    (* <= module G *)
  in
  let module H = G (* ? module G *) in
  H.x
