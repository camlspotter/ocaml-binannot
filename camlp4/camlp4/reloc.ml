(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open MLast;

value option_map f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value rec ctyp floc sh =
  self where rec self =
    fun
    [ TyAcc loc x1 x2 -> TyAcc (floc loc) (self x1) (self x2)
    | TyAli loc x1 x2 -> TyAli (floc loc) (self x1) (self x2)
    | TyAny loc -> TyAny (floc loc)
    | TyApp loc x1 x2 -> TyApp (floc loc) (self x1) (self x2)
    | TyArr loc x1 x2 -> TyArr (floc loc) (self x1) (self x2)
    | TyCls loc x1 -> TyCls (floc loc) x1
    | TyLab loc x1 x2 -> TyLab (floc loc) x1 (self x2)
    | TyLid loc x1 -> TyLid (floc loc) x1
    | TyMan loc x1 x2 -> TyMan (floc loc) (self x1) (self x2)
    | TyObj loc x1 x2 ->
        TyObj (floc loc) (List.map (fun (x1, x2) -> (x1, self x2)) x1) x2
    | TyOlb loc x1 x2 -> TyOlb (floc loc) x1 (self x2)
    | TyQuo loc x1 -> TyQuo (floc loc) x1
    | TyRec loc x1 ->
        TyRec (floc loc) (List.map (fun (x1, x2, x3) -> (x1, x2, self x3)) x1)
    | TySum loc x1 ->
        TySum (floc loc)
          (List.map (fun (x1, x2) -> (x1, List.map self x2)) x1)
    | TyTup loc x1 -> TyTup (floc loc) (List.map self x1)
    | TyUid loc x1 -> TyUid (floc loc) x1
    | TyVrn loc x1 x2 ->
        TyVrn (floc loc)
          (List.map (fun (x1, x2, x3) -> (x1, x2, List.map self x3)) x1) x2
    | TyXnd loc x1 x2 -> TyXnd (floc loc) x1 (self x2) ]
;

value class_infos a floc sh x =
  {ciLoc = floc x.ciLoc; ciVir = x.ciVir;
   ciPrm =
     let (x1, x2) = x.ciPrm in
     (floc x1, x2);
   ciNam = x.ciNam; ciExp = a floc sh x.ciExp}
;

value rec patt floc sh =
  self where rec self =
    fun
    [ PaAcc loc x1 x2 -> PaAcc (floc loc) (self x1) (self x2)
    | PaAli loc x1 x2 -> PaAli (floc loc) (self x1) (self x2)
    | PaAnt loc x1 ->
        patt (fun (p1, p2) -> (sh + fst loc + p1, sh + fst loc + p2)) 0 x1
    | PaAny loc -> PaAny (floc loc)
    | PaApp loc x1 x2 -> PaApp (floc loc) (self x1) (self x2)
    | PaArr loc x1 -> PaArr (floc loc) (List.map self x1)
    | PaChr loc x1 -> PaChr (floc loc) x1
    | PaInt loc x1 -> PaInt (floc loc) x1
    | PaFlo loc x1 -> PaFlo (floc loc) x1
    | PaLab loc x1 x2 -> PaLab (floc loc) x1 (self x2)
    | PaLid loc x1 -> PaLid (floc loc) x1
    | PaOlb loc x1 x2 x3 ->
        PaOlb (floc loc) x1 (self x2) (option_map (expr floc sh) x3)
    | PaOrp loc x1 x2 -> PaOrp (floc loc) (self x1) (self x2)
    | PaRng loc x1 x2 -> PaRng (floc loc) (self x1) (self x2)
    | PaRec loc x1 ->
        PaRec (floc loc) (List.map (fun (x1, x2) -> (self x1, self x2)) x1)
    | PaStr loc x1 -> PaStr (floc loc) x1
    | PaTup loc x1 -> PaTup (floc loc) (List.map self x1)
    | PaTyc loc x1 x2 -> PaTyc (floc loc) (self x1) (ctyp floc sh x2)
    | PaTyp loc x1 -> PaTyp (floc loc) x1
    | PaUid loc x1 -> PaUid (floc loc) x1
    | PaVrn loc x1 -> PaVrn (floc loc) x1
    | PaXnd loc x1 x2 -> PaXnd (floc loc) x1 (self x2) ]
and expr floc sh =
  self where rec self =
    fun
    [ ExAcc loc x1 x2 -> ExAcc (floc loc) (self x1) (self x2)
    | ExAnt loc x1 ->
        expr (fun (p1, p2) -> (sh + fst loc + p1, sh + fst loc + p2)) 0 x1
    | ExApp loc x1 x2 -> ExApp (floc loc) (self x1) (self x2)
    | ExAre loc x1 x2 -> ExAre (floc loc) (self x1) (self x2)
    | ExArr loc x1 -> ExArr (floc loc) (List.map self x1)
    | ExAss loc x1 x2 -> ExAss (floc loc) (self x1) (self x2)
    | ExChr loc x1 -> ExChr (floc loc) x1
    | ExCoe loc x1 x2 x3 ->
        ExCoe (floc loc) (self x1) (option_map (ctyp floc sh) x2)
          (ctyp floc sh x3)
    | ExFlo loc x1 -> ExFlo (floc loc) x1
    | ExFor loc x1 x2 x3 x4 x5 ->
        ExFor (floc loc) x1 (self x2) (self x3) x4 (List.map self x5)
    | ExFun loc x1 ->
        ExFun (floc loc)
          (List.map
             (fun (x1, x2, x3) ->
                (patt floc sh x1, option_map self x2, self x3))
             x1)
    | ExIfe loc x1 x2 x3 -> ExIfe (floc loc) (self x1) (self x2) (self x3)
    | ExInt loc x1 -> ExInt (floc loc) x1
    | ExLab loc x1 x2 -> ExLab (floc loc) x1 (self x2)
    | ExLet loc x1 x2 x3 ->
        ExLet (floc loc) x1
          (List.map (fun (x1, x2) -> (patt floc sh x1, self x2)) x2) (self x3)
    | ExLid loc x1 -> ExLid (floc loc) x1
    | ExLmd loc x1 x2 x3 ->
        ExLmd (floc loc) x1 (module_expr floc sh x2) (self x3)
    | ExMat loc x1 x2 ->
        ExMat (floc loc) (self x1)
          (List.map
             (fun (x1, x2, x3) ->
                (patt floc sh x1, option_map self x2, self x3))
             x2)
    | ExNew loc x1 -> ExNew (floc loc) x1
    | ExOlb loc x1 x2 -> ExOlb (floc loc) x1 (self x2)
    | ExOvr loc x1 ->
        ExOvr (floc loc) (List.map (fun (x1, x2) -> (x1, self x2)) x1)
    | ExRec loc x1 x2 ->
        ExRec (floc loc)
          (List.map (fun (x1, x2) -> (patt floc sh x1, self x2)) x1)
          (option_map self x2)
    | ExSeq loc x1 -> ExSeq (floc loc) (List.map self x1)
    | ExSnd loc x1 x2 -> ExSnd (floc loc) (self x1) x2
    | ExSte loc x1 x2 -> ExSte (floc loc) (self x1) (self x2)
    | ExStr loc x1 -> ExStr (floc loc) x1
    | ExTry loc x1 x2 ->
        ExTry (floc loc) (self x1)
          (List.map
             (fun (x1, x2, x3) ->
                (patt floc sh x1, option_map self x2, self x3))
             x2)
    | ExTup loc x1 -> ExTup (floc loc) (List.map self x1)
    | ExTyc loc x1 x2 -> ExTyc (floc loc) (self x1) (ctyp floc sh x2)
    | ExUid loc x1 -> ExUid (floc loc) x1
    | ExVrn loc x1 -> ExVrn (floc loc) x1
    | ExWhi loc x1 x2 -> ExWhi (floc loc) (self x1) (List.map self x2)
    | ExXnd loc x1 x2 -> ExXnd (floc loc) x1 (self x2) ]
and module_type floc sh =
  self where rec self =
    fun
    [ MtAcc loc x1 x2 -> MtAcc (floc loc) (self x1) (self x2)
    | MtApp loc x1 x2 -> MtApp (floc loc) (self x1) (self x2)
    | MtFun loc x1 x2 x3 -> MtFun (floc loc) x1 (self x2) (self x3)
    | MtLid loc x1 -> MtLid (floc loc) x1
    | MtSig loc x1 -> MtSig (floc loc) (List.map (sig_item floc sh) x1)
    | MtUid loc x1 -> MtUid (floc loc) x1
    | MtWit loc x1 x2 ->
        MtWit (floc loc) (self x1) (List.map (with_constr floc sh) x2) ]
and sig_item floc sh =
  self where rec self =
    fun
    [ SgCls loc x1 ->
        SgCls (floc loc) (List.map (class_infos class_type floc sh) x1)
    | SgClt loc x1 ->
        SgClt (floc loc) (List.map (class_infos class_type floc sh) x1)
    | SgDcl loc x1 -> SgDcl (floc loc) (List.map self x1)
    | SgDir loc x1 x2 -> SgDir (floc loc) x1 x2
    | SgExc loc x1 x2 -> SgExc (floc loc) x1 (List.map (ctyp floc sh) x2)
    | SgExt loc x1 x2 x3 -> SgExt (floc loc) x1 (ctyp floc sh x2) x3
    | SgInc loc x1 -> SgInc (floc loc) (module_type floc sh x1)
    | SgMod loc x1 x2 -> SgMod (floc loc) x1 (module_type floc sh x2)
    | SgMty loc x1 x2 -> SgMty (floc loc) x1 (module_type floc sh x2)
    | SgOpn loc x1 -> SgOpn (floc loc) x1
    | SgTyp loc x1 ->
        SgTyp (floc loc)
          (List.map
             (fun (x1, x2, x3, x4) ->
                (x1, x2, ctyp floc sh x3,
                 List.map (fun (x1, x2) -> (ctyp floc sh x1, ctyp floc sh x2))
                   x4))
             x1)
    | SgVal loc x1 x2 -> SgVal (floc loc) x1 (ctyp floc sh x2) ]
and with_constr floc sh =
  self where rec self =
    fun
    [ WcTyp loc x1 x2 x3 -> WcTyp (floc loc) x1 x2 (ctyp floc sh x3)
    | WcMod loc x1 x2 -> WcMod (floc loc) x1 (module_type floc sh x2) ]
and module_expr floc sh =
  self where rec self =
    fun
    [ MeAcc loc x1 x2 -> MeAcc (floc loc) (self x1) (self x2)
    | MeApp loc x1 x2 -> MeApp (floc loc) (self x1) (self x2)
    | MeFun loc x1 x2 x3 ->
        MeFun (floc loc) x1 (module_type floc sh x2) (self x3)
    | MeStr loc x1 -> MeStr (floc loc) (List.map (str_item floc sh) x1)
    | MeTyc loc x1 x2 -> MeTyc (floc loc) (self x1) (module_type floc sh x2)
    | MeUid loc x1 -> MeUid (floc loc) x1 ]
and str_item floc sh =
  self where rec self =
    fun
    [ StCls loc x1 ->
        StCls (floc loc) (List.map (class_infos class_expr floc sh) x1)
    | StClt loc x1 ->
        StClt (floc loc) (List.map (class_infos class_type floc sh) x1)
    | StDcl loc x1 -> StDcl (floc loc) (List.map self x1)
    | StDir loc x1 x2 -> StDir (floc loc) x1 x2
    | StExc loc x1 x2 -> StExc (floc loc) x1 (List.map (ctyp floc sh) x2)
    | StExp loc x1 -> StExp (floc loc) (expr floc sh x1)
    | StExt loc x1 x2 x3 -> StExt (floc loc) x1 (ctyp floc sh x2) x3
    | StInc loc x1 -> StInc (floc loc) (module_expr floc sh x1)
    | StMod loc x1 x2 -> StMod (floc loc) x1 (module_expr floc sh x2)
    | StMty loc x1 x2 -> StMty (floc loc) x1 (module_type floc sh x2)
    | StOpn loc x1 -> StOpn (floc loc) x1
    | StTyp loc x1 ->
        StTyp (floc loc)
          (List.map
             (fun (x1, x2, x3, x4) ->
                (x1, x2, ctyp floc sh x3,
                 List.map (fun (x1, x2) -> (ctyp floc sh x1, ctyp floc sh x2))
                   x4))
             x1)
    | StVal loc x1 x2 ->
        StVal (floc loc) x1
          (List.map (fun (x1, x2) -> (patt floc sh x1, expr floc sh x2)) x2) ]
and class_type floc sh =
  self where rec self =
    fun
    [ CtCon loc x1 x2 -> CtCon (floc loc) x1 (List.map (ctyp floc sh) x2)
    | CtFun loc x1 x2 -> CtFun (floc loc) (ctyp floc sh x1) (self x2)
    | CtSig loc x1 x2 ->
        CtSig (floc loc) (option_map (ctyp floc sh) x1)
          (List.map (class_sig_item floc sh) x2)
    | CtXnd loc x1 x2 -> CtXnd (floc loc) x1 (self x2) ]
and class_sig_item floc sh =
  self where rec self =
    fun
    [ CgCtr loc x1 x2 -> CgCtr (floc loc) (ctyp floc sh x1) (ctyp floc sh x2)
    | CgDcl loc x1 -> CgDcl (floc loc) (List.map (class_sig_item floc sh) x1)
    | CgInh loc x1 -> CgInh (floc loc) (class_type floc sh x1)
    | CgMth loc x1 x2 x3 -> CgMth (floc loc) x1 x2 (ctyp floc sh x3)
    | CgVal loc x1 x2 x3 -> CgVal (floc loc) x1 x2 (ctyp floc sh x3)
    | CgVir loc x1 x2 x3 -> CgVir (floc loc) x1 x2 (ctyp floc sh x3) ]
and class_expr floc sh =
  self where rec self =
    fun
    [ CeApp loc x1 x2 ->
        CeApp (floc loc) (self x1) (List.map (expr floc sh) x2)
    | CeCon loc x1 x2 -> CeCon (floc loc) x1 (List.map (ctyp floc sh) x2)
    | CeFun loc x1 x2 -> CeFun (floc loc) (patt floc sh x1) (self x2)
    | CeLet loc x1 x2 x3 ->
        CeLet (floc loc) x1
          (List.map (fun (x1, x2) -> (patt floc sh x1, expr floc sh x2)) x2)
          (self x3)
    | CeStr loc x1 x2 ->
        CeStr (floc loc) (option_map (patt floc sh) x1)
          (List.map (class_str_item floc sh) x2)
    | CeTyc loc x1 x2 -> CeTyc (floc loc) (self x1) (class_type floc sh x2)
    | CeXnd loc x1 x2 -> CeXnd (floc loc) x1 (self x2) ]
and class_str_item floc sh =
  self where rec self =
    fun
    [ CrCtr loc x1 x2 -> CrCtr (floc loc) (ctyp floc sh x1) (ctyp floc sh x2)
    | CrDcl loc x1 -> CrDcl (floc loc) (List.map (class_str_item floc sh) x1)
    | CrInh loc x1 x2 -> CrInh (floc loc) (class_expr floc sh x1) x2
    | CrIni loc x1 -> CrIni (floc loc) (expr floc sh x1)
    | CrMth loc x1 x2 x3 -> CrMth (floc loc) x1 x2 (expr floc sh x3)
    | CrVal loc x1 x2 x3 -> CrVal (floc loc) x1 x2 (expr floc sh x3)
    | CrVir loc x1 x2 x3 -> CrVir (floc loc) x1 x2 (ctyp floc sh x3) ]
;
