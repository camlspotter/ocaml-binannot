(* builtin to handle callback association to widget *)
let own_set ?command =
selection_ownset_icccm_optionals ?command (fun opts w ->
tkCommand [|TkToken"selection";
         TkToken"own";
         TkTokenList opts;
        cCAMLtoTKwidget w|])

