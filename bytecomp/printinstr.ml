(* Pretty-print lists of instructions *)

open Format
open Lambda
open Instruct


let instruction = function
    Klabel lbl -> print_string "L"; print_int lbl; print_string ":"
  | Kacc n -> print_string "\tacc "; print_int n
  | Kenvacc n -> print_string "\tenvacc "; print_int n
  | Kpush -> print_string "\tpush"
  | Kpop n -> print_string "\tpop "; print_int n
  | Kassign n -> print_string "\tassign "; print_int n
  | Kpush_retaddr lbl -> print_string "\tpush_retaddr L"; print_int lbl
  | Kapply n -> print_string "\tapply "; print_int n
  | Kappterm(n, m) ->
      print_string "\tappterm "; print_int n; print_string ", "; print_int m
  | Kreturn n -> print_string "\treturn "; print_int n
  | Krestart -> print_string "\trestart"
  | Kgrab n -> print_string "\tgrab "; print_int n
  | Kclosure(lbl, n) ->
      print_string "\tclosure L"; print_int lbl; print_string ", "; print_int n
  | Kclosurerec(lbl, n) ->
      print_string "\tclosurerec L"; print_int lbl;
      print_string ", "; print_int n
  | Kgetglobal id -> print_string "\tgetglobal "; Ident.print id
  | Ksetglobal id -> print_string "\tsetglobal "; Ident.print id
  | Kconst cst ->
      open_hovbox 10; print_string "\tconst"; print_space();
      Printlambda.structured_constant cst; close_box()
  | Kmakeblock(n, m) ->
      print_string "\tmakeblock "; print_int n; print_string ", "; print_int m
  | Kgetfield n -> print_string "\tgetfield "; print_int n
  | Ksetfield n -> print_string "\tsetfield "; print_int n
  | Kdummy n -> print_string "\tdummy "; print_int n
  | Kupdate n -> print_string "\tupdate"; print_int n
  | Kvectlength -> print_string "\tvectlength"
  | Kgetvectitem -> print_string "\tgetvectitem"
  | Ksetvectitem -> print_string "\tsetvectitem"
  | Kgetstringchar -> print_string "\tgetstringchar"
  | Ksetstringchar -> print_string "\tsetstringchar"
  | Kbranch lbl -> print_string "\tbranch L"; print_int lbl
  | Kbranchif lbl -> print_string "\tbranchif L"; print_int lbl
  | Kbranchifnot lbl -> print_string "\tbranchifnot L"; print_int lbl
  | Kstrictbranchif lbl -> print_string "\tstrictbranchif L"; print_int lbl
  | Kstrictbranchifnot lbl ->
      print_string "\tstrictbranchifnot L"; print_int lbl
  | Kswitch(consts, blocks) ->
      open_hovbox 10;
      print_string "\tswitch";
      Array.iter (fun lbl -> print_space(); print_int lbl) consts;
      print_string "/";
      Array.iter (fun lbl -> print_space(); print_int lbl) blocks;
      close_box()
  | Ktranslate tbl ->
      open_hovbox 10;
      print_string "\ttranslate";
      Array.iter
        (fun (lo, hi, ofs) ->
          print_space(); print_int lo; print_string "/";
          print_int hi; print_string "/"; print_int ofs)
        tbl;
      close_box()
  | Kboolnot -> print_string "\tboolnot"
  | Kpushtrap lbl -> print_string "\tpushtrap L"; print_int lbl
  | Kpoptrap -> print_string "\tpoptrap"
  | Kraise -> print_string "\traise"
  | Kcheck_signals -> print_string "\tcheck_signals"
  | Kccall(s, n) ->
      print_string "\tccall "; print_string s; print_string ", "; print_int n
  | Knegint -> print_string "\tnegint"
  | Kaddint -> print_string "\taddint"
  | Ksubint -> print_string "\tsubint"
  | Kmulint -> print_string "\tmulint"
  | Kdivint -> print_string "\tdivint"
  | Kmodint -> print_string "\tmodint"
  | Kandint -> print_string "\tandint"
  | Korint -> print_string "\torint"
  | Kxorint -> print_string "\txorint"
  | Klslint -> print_string "\tlslint"
  | Klsrint -> print_string "\tlsrint"
  | Kasrint -> print_string "\tasrint"
  | Kintcomp Ceq -> print_string "\teqint"
  | Kintcomp Cneq -> print_string "\tneqint"
  | Kintcomp Clt -> print_string "\tltint"
  | Kintcomp Cgt -> print_string "\tgtint"
  | Kintcomp Cle -> print_string "\tleint"
  | Kintcomp Cge -> print_string "\tgeint"
  | Koffsetint n -> print_string "\toffsetint "; print_int n
  | Koffsetref n -> print_string "\toffsetref "; print_int n
  | Kstop -> print_string "\tstop"

let rec instruction_list = function
    [] -> ()
  | Klabel lbl :: il ->
      print_string "L"; print_int lbl; print_string ":"; instruction_list il
  | instr :: il ->
      instruction instr; print_space(); instruction_list il
 
let instrlist il =
  open_vbox 0;
  instruction_list il;
  close_box()
