class (* c => *) c = object end (* <= c *)  (* CR jfuruse: improve ? *)

class nc = object
  inherit let _x = 1 in c (* ? c *)
end
