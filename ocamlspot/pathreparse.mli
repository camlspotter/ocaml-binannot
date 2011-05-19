open Spotapi

val get :
  string                     (* source file name *)
  -> Region.t      (* the spot region *)
  -> Position.t    (* cursor pos in the region *)
  -> Path.t         (* the path found at the region *)
  -> (Path.t * Region.t) option  (* sub path found *)
