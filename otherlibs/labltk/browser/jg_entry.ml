(* $Id$ *)

open Tk

let create ?:command ?:width ?:textvariable parent =
  let ew = Entry.create parent ?:width ?:textvariable in
  Jg_bind.enter_focus ew;
  begin match command with Some command ->
    bind ew events:[`KeyPressDetail "Return"]
      action:(fun _ -> command (Entry.get ew))
  | None -> ()
  end;
  ew
