(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Asttypes
open Parsetree
open Longident
open Location

type stream_pattern_component =
    Spat_term of pattern * expression option
  | Spat_nterm of pattern * expression
  | Spat_sterm of pattern
type stream_expr_component =
    Sexp_term of expression
  | Sexp_nterm of expression

let mkpat d = { ppat_desc = d; ppat_loc = symbol_loc() }
let mkexp d = { pexp_desc = d; pexp_loc = symbol_loc() }
let eloc loc e = { pexp_desc = e; pexp_loc = loc }
let ploc loc p = { ppat_desc = p; ppat_loc = loc }

let spat = Ppat_var "%strm"
let sexp = Pexp_ident (Lident "%strm")
let eval x = mkexp (Pexp_ident (Ldot (Lident "Stream", x)))
let econ c x = mkexp (Pexp_construct (Ldot (Lident "Stream", c), x))
let pcon c x = mkpat (Ppat_construct (Ldot (Lident "Stream", c), x))
let afun f x =
  mkexp (Pexp_apply (mkexp (Pexp_ident (Ldot (Lident "Stream", f))), x))
let araise c x =
  mkexp (Pexp_apply (mkexp (Pexp_ident (Lident "raise")), [econ c x]))
let esome x = mkexp (Pexp_construct (Lident "Some", Some x))


(* parsers *)

let stream_pattern_component skont =
  function
    Spat_term (p, None) ->
      (afun "peek" [mkexp sexp],
       p, mkexp (Pexp_sequence (afun "junk" [mkexp sexp], skont)))
  | Spat_term (p, Some e) ->
      (afun "peek" [mkexp sexp],
       p,
       mkexp
         (Pexp_when
            (e, mkexp(Pexp_sequence (afun "junk" [mkexp sexp], skont)))))
  | Spat_nterm (p, e) ->
      (mkexp
         (Pexp_try
            (esome (mkexp (Pexp_apply (e, [mkexp sexp]))),
             [(pcon "Parse_failure" None,
               mkexp (Pexp_construct (Lident "None", None)))])),
       p, skont)
  | Spat_sterm p ->
      (esome (mkexp sexp), p, skont)

let rec stream_pattern epo e is_first =
  function
    [] ->
      begin match epo with
        Some ep ->
          mkexp (Pexp_let (Nonrecursive, [(ep, afun "count" [mkexp sexp])], e))
      | _ -> e
      end
  | (spc, err) :: spcl ->
      let skont = stream_pattern epo e false spcl in
      let (tst, p, e) = stream_pattern_component skont spc in
      let ckont =
        if is_first then araise "Parse_failure" None
        else
          let str =
            match err with
              Some estr -> estr
            | _ -> mkexp (Pexp_constant (Const_string ""))
          in
          araise "Parse_error" (Some str)
      in
      mkexp
        (Pexp_match
           (tst,
            [(ploc p.ppat_loc (Ppat_construct (Lident "Some", Some p)), e);
             (mkpat Ppat_any, ckont)]))

let parser_case (spcl, epo, e) =
  stream_pattern epo e true spcl

let parser_cases pc =
  List.fold_right
    (fun pc kont ->
       let e = parser_case pc in
       eloc e.pexp_loc (Pexp_try (e, [(pcon "Parse_failure" None, kont)])))
    pc
    (araise "Parse_failure" None)
  
let bp_parser_cases (bpo, pc) =
  let pc = parser_cases pc in
  let e =
    match bpo with
      Some bp ->
        mkexp (Pexp_let (Nonrecursive, [(bp, afun "count" [mkexp sexp])], pc))
    | None -> pc
  in
  mkexp (Pexp_function [(mkpat spat, e)])

let cparser (bpo, pc) = bp_parser_cases (bpo, pc)


(* streams *)

let lazy e = mkexp (Pexp_function [(mkpat Ppat_any, e)])

let rec cstream =
  function
    [] -> eval "sempty"
  | Sexp_term e :: secl -> afun "scons" [lazy e; cstream secl]
  | Sexp_nterm e :: secl -> afun "sapp" [lazy e; cstream secl]
