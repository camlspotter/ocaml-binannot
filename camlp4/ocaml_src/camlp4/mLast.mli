(* camlp4r *)
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

(* Module [MLast]: abstract syntax tree *)

type loc = int * int;;

type ctyp =
    TyAcc of loc * ctyp * ctyp
  | TyAli of loc * ctyp * ctyp
  | TyAny of loc
  | TyApp of loc * ctyp * ctyp
  | TyArr of loc * ctyp * ctyp
  | TyCls of loc * string list
  | TyLab of loc * string * ctyp
  | TyLid of loc * string
  | TyMan of loc * ctyp * ctyp
  | TyObj of loc * (string * ctyp) list * bool
  | TyOlb of loc * string * ctyp
  | TyQuo of loc * string
  | TyRec of loc * (string * bool * ctyp) list
  | TySum of loc * (string * ctyp list) list
  | TyTup of loc * ctyp list
  | TyUid of loc * string
  | TyVrn of loc * row_field list * string list option option
  | TyXnd of loc * string * ctyp
and row_field = RfTag of string * bool * ctyp list | RfInh of ctyp
;;

type 'a class_infos =
  { ciLoc : loc;
    ciVir : bool;
    ciPrm : loc * (string * (bool * bool)) list;
    ciNam : string;
    ciExp : 'a }
;;

type patt =
    PaAcc of loc * patt * patt
  | PaAli of loc * patt * patt
  | PaAnt of loc * patt
  | PaAny of loc
  | PaApp of loc * patt * patt
  | PaArr of loc * patt list
  | PaChr of loc * string
  | PaInt of loc * string
  | PaFlo of loc * string
  | PaLab of loc * string * patt
  | PaLid of loc * string
  | PaOlb of loc * string * patt * expr option
  | PaOrp of loc * patt * patt
  | PaRng of loc * patt * patt
  | PaRec of loc * (patt * patt) list
  | PaStr of loc * string
  | PaTup of loc * patt list
  | PaTyc of loc * patt * ctyp
  | PaTyp of loc * string list
  | PaUid of loc * string
  | PaVrn of loc * string
  | PaXnd of loc * string * patt
and expr =
    ExAcc of loc * expr * expr
  | ExAnt of loc * expr
  | ExApp of loc * expr * expr
  | ExAre of loc * expr * expr
  | ExArr of loc * expr list
  | ExAss of loc * expr * expr
  | ExChr of loc * string
  | ExCoe of loc * expr * ctyp option * ctyp
  | ExFlo of loc * string
  | ExFor of loc * string * expr * expr * bool * expr list
  | ExFun of loc * (patt * expr option * expr) list
  | ExIfe of loc * expr * expr * expr
  | ExInt of loc * string
  | ExLab of loc * string * expr
  | ExLet of loc * bool * (patt * expr) list * expr
  | ExLid of loc * string
  | ExLmd of loc * string * module_expr * expr
  | ExMat of loc * expr * (patt * expr option * expr) list
  | ExNew of loc * string list
  | ExOlb of loc * string * expr
  | ExOvr of loc * (string * expr) list
  | ExRec of loc * (patt * expr) list * expr option
  | ExSeq of loc * expr list
  | ExSnd of loc * expr * string
  | ExSte of loc * expr * expr
  | ExStr of loc * string
  | ExTry of loc * expr * (patt * expr option * expr) list
  | ExTup of loc * expr list
  | ExTyc of loc * expr * ctyp
  | ExUid of loc * string
  | ExVrn of loc * string
  | ExWhi of loc * expr * expr list
  | ExXnd of loc * string * expr
and module_type =
    MtAcc of loc * module_type * module_type
  | MtApp of loc * module_type * module_type
  | MtFun of loc * string * module_type * module_type
  | MtLid of loc * string
  | MtSig of loc * sig_item list
  | MtUid of loc * string
  | MtWit of loc * module_type * with_constr list
and sig_item =
    SgCls of loc * class_type class_infos list
  | SgClt of loc * class_type class_infos list
  | SgDcl of loc * sig_item list
  | SgDir of loc * string * expr option
  | SgExc of loc * string * ctyp list
  | SgExt of loc * string * ctyp * string list
  | SgInc of loc * module_type
  | SgMod of loc * string * module_type
  | SgMty of loc * string * module_type
  | SgOpn of loc * string list
  | SgTyp of loc * type_decl list
  | SgVal of loc * string * ctyp
and with_constr =
    WcTyp of loc * string list * (string * (bool * bool)) list * ctyp
  | WcMod of loc * string list * module_type
and module_expr =
    MeAcc of loc * module_expr * module_expr
  | MeApp of loc * module_expr * module_expr
  | MeFun of loc * string * module_type * module_expr
  | MeStr of loc * str_item list
  | MeTyc of loc * module_expr * module_type
  | MeUid of loc * string
and str_item =
    StCls of loc * class_expr class_infos list
  | StClt of loc * class_type class_infos list
  | StDcl of loc * str_item list
  | StDir of loc * string * expr option
  | StExc of loc * string * ctyp list
  | StExp of loc * expr
  | StExt of loc * string * ctyp * string list
  | StInc of loc * module_expr
  | StMod of loc * string * module_expr
  | StMty of loc * string * module_type
  | StOpn of loc * string list
  | StTyp of loc * type_decl list
  | StVal of loc * bool * (patt * expr) list
and type_decl =
  string * (string * (bool * bool)) list * ctyp * (ctyp * ctyp) list
and class_type =
    CtCon of loc * string list * ctyp list
  | CtFun of loc * ctyp * class_type
  | CtSig of loc * ctyp option * class_sig_item list
  | CtXnd of loc * string * class_type
and class_sig_item =
    CgCtr of loc * ctyp * ctyp
  | CgDcl of loc * class_sig_item list
  | CgInh of loc * class_type
  | CgMth of loc * string * bool * ctyp
  | CgVal of loc * string * bool * ctyp
  | CgVir of loc * string * bool * ctyp
and class_expr =
    CeApp of loc * class_expr * expr list
  | CeCon of loc * string list * ctyp list
  | CeFun of loc * patt * class_expr
  | CeLet of loc * bool * (patt * expr) list * class_expr
  | CeStr of loc * patt option * class_str_item list
  | CeTyc of loc * class_expr * class_type
  | CeXnd of loc * string * class_expr
and class_str_item =
    CrCtr of loc * ctyp * ctyp
  | CrDcl of loc * class_str_item list
  | CrInh of loc * class_expr * string option
  | CrIni of loc * expr
  | CrMth of loc * string * bool * expr
  | CrVal of loc * string * bool * expr
  | CrVir of loc * string * bool * ctyp
;;

external loc_of_ctyp : ctyp -> loc = "%field0";;
external loc_of_patt : patt -> loc = "%field0";;
external loc_of_expr : expr -> loc = "%field0";;
external loc_of_module_type : module_type -> loc = "%field0";;
external loc_of_module_expr : module_expr -> loc = "%field0";;
external loc_of_sig_item : sig_item -> loc = "%field0";;
external loc_of_str_item : str_item -> loc = "%field0";;
