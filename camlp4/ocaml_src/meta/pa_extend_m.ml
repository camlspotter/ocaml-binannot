(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

open Pa_extend;;

let psymbol p s t =
  let symb = {used = []; text = s; styp = fun _ -> t} in
  {pattern = Some p; symbol = symb}
;;

Grammar.extend
  [Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e),
   Some (Gramext.Level "top"),
   [None, Some Gramext.NonA,
    [[Gramext.srules
        [[Gramext.Stoken ("UIDENT", "SLIST1")],
         Gramext.action (fun _ (loc : int * int) -> (true : 'e__1));
         [Gramext.Stoken ("UIDENT", "SLIST0")],
         Gramext.action (fun _ (loc : int * int) -> (false : 'e__1))];
      Gramext.Sself;
      Gramext.Sopt
        (Gramext.srules
           [[Gramext.Stoken ("UIDENT", "SEP");
             Gramext.Snterm
               (Grammar.Entry.obj (symbol : 'symbol Grammar.Entry.e))],
            Gramext.action
              (fun (t : 'symbol) _ (loc : int * int) -> (t : 'e__2))])],
     Gramext.action
       (fun (sep : 'e__2 option) (s : 'symbol) (min : 'e__1)
          (loc : int * int) ->
          (let used =
             match sep with
               Some symb ->
                 mk_name loc (MLast.ExLid (loc, "anti")) ::
                   (symb.used @ s.used)
             | None -> s.used
           in
           let text n =
             let rl =
               let r1 =
                 let prod =
                   let n = mk_name loc (MLast.ExLid (loc, "anti_list")) in
                   [psymbol (MLast.PaLid (loc, "a")) (snterm loc n None)
                      (MLast.TyQuo (loc, "anti_list"))]
                 in
                 let act = MLast.ExLid (loc, "a") in
                 {prod = prod; action = Some act}
               in
               let r2 =
                 let psymb =
                   let symb =
                     {used = []; text = slist loc min sep s;
                      styp =
                        fun n ->
                          MLast.TyApp
                            (loc, MLast.TyLid (loc, "list"), s.styp n)}
                   in
                   let patt = MLast.PaLid (loc, "l") in
                   {pattern = Some patt; symbol = symb}
                 in
                 let act =
                   MLast.ExApp
                     (loc, MLast.ExLid (loc, "list"), MLast.ExLid (loc, "l"))
                 in
                 {prod = [psymb]; action = Some act}
               in
               [r1; r2]
             in
             srules loc "anti" rl n
           in
           {used = used; text = text;
            styp = fun _ -> MLast.TyLid (loc, "ast")} :
           'symbol))]]];;
