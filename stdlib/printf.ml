(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

external format_int: string -> int -> string = "caml_format_int"
external format_int32: string -> int32 -> string = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
                         = "caml_nativeint_format"
external format_int64: string -> int64 -> string = "caml_int64_format"
external format_float: string -> float -> string = "caml_format_float"

external format_to_string: ('a, 'b, 'c, 'd) format4 -> string = "%identity"

type sz;;

external sz_of_int : int -> sz = "%identity";;
external int_of_sz : sz -> int = "%identity";;

let succs sz = sz_of_int (succ (int_of_sz sz));;

let bad_conversion fmt i c =
  invalid_arg
    ("printf: bad conversion %" ^ String.make 1 c ^ ", at char number " ^
     string_of_int i ^ " in format string ``" ^ fmt ^ "''");;

let incomplete_format fmt =
  invalid_arg
    ("printf: premature end of format string ``" ^ fmt ^ "''");;

(* Parses a format to return the specified length and the padding direction. *)
let parse_format fmt =
  let rec parse neg i =
    if i >= String.length fmt then (0, neg) else
    match String.unsafe_get fmt i with
    | '1'..'9' ->
        (int_of_string (String.sub fmt i (String.length fmt - i - 1)),
         neg)
    | '-' ->
        parse true (succ i)
    | _ ->
        parse neg (succ i) in
  try parse false 1 with Failure _ -> bad_conversion fmt 0 's'

(* Pad a (sub) string into a blank string of length [p],
   on the right if [neg] is true, on the left otherwise. *)
let pad_string pad_char p neg s i len =
  if p = len && i = 0 then s else
  if p <= len then String.sub s i len else
  let res = String.make p pad_char in
  if neg
  then String.blit s i res 0 len
  else String.blit s i res (p - len) len;
  res

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do: ignore other flags (#, +, etc)? *)
let format_string fmt s =
  let (p, neg) = parse_format fmt in
  pad_string ' ' p neg s 0 (String.length s)

(* Extract a %format from [fmt] between [start] and [stop] inclusive.
   '*' in the format are replaced by integers taken from the [widths] list.
   The function is somewhat optimized for the "no *" case. *)

let extract_format fmt start stop widths =
  match widths with
  | [] -> String.sub fmt start (stop - start + 1)
  | _  ->
      let b = Buffer.create (stop - start + 10) in
      let rec fill_format i w =
        if i > stop then Buffer.contents b else
          match (String.unsafe_get fmt i, w) with
          | ('*', h :: t) ->
              Buffer.add_string b (string_of_int h); fill_format (succ i) t
          | ('*', []) ->
              assert false (* should not happen *)
          | (c, _) ->
              Buffer.add_char b c; fill_format (succ i) w
      in fill_format start (List.rev widths)

let format_int_with_conv conv fmt i =
   match conv with
   | 'n' | 'N' -> fmt.[String.length fmt - 1] <- 'u'; format_int fmt i
   | _ -> format_int fmt i

(* Returns the position of the last character of the meta format
   string, starting from position [i], inside a given format [fmt].
   According to the character [conv], the meta format string is
   enclosed by the delimitors %{ and %} (when [conv = '{']) or %( and
   %) (when [conv = '(']). Hence, [sub_format] returns the index of
   the character ')' or '}' that ends the meta format, according to
   the character [conv]. *)
let sub_format incomplete_format bad_conversion conv fmt i =
  let len = String.length fmt in
  let rec sub_fmt c i =
    let close = if c = '(' then ')' else '}' in
    let rec sub j =
       if j >= len then incomplete_format fmt else
       match fmt.[j] with
       | '%' -> sub_sub (succ j)
       | _ -> sub (succ j)
    and sub_sub j =
       if j >= len then incomplete_format fmt else
       match fmt.[j] with
       | '(' | '{' as c ->
         let j = sub_fmt c (succ j) in sub (succ j)
       | ')' | '}' as c ->
         if c = close then j else bad_conversion fmt i c
       | _ -> sub (succ j) in
    sub i in
  sub_fmt conv i;;

let sub_format_for_printf = sub_format incomplete_format bad_conversion;;

let iter_format_args fmt add_conv add_char =
  let len = String.length fmt in
  let rec scan_flags skip i =
    if i >= len then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '*' -> scan_flags skip (add_conv skip i 'i')
    | '#' | '-' | ' ' | '+' -> scan_flags skip (succ i)
    | '_' -> scan_flags true (succ i)
    | '0'..'9'
    | '.'  -> scan_flags skip (succ i)
    | _ -> scan_conv skip i
  and scan_conv skip i =
    if i >= len then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '%' | '!' -> succ i
    | 's' | 'S' | '[' -> add_conv skip i 's'
    | 'c' | 'C' -> add_conv skip i 'c'
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' -> add_conv skip i 'i'
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> add_conv skip i 'f'
    | 'B' | 'b' -> add_conv skip i 'B'
    | 'a' | 't' as conv -> add_conv skip i conv
    | 'l' | 'n' | 'L' as conv ->
        let j = succ i in
        if j >= len then add_conv skip i 'i' else begin
          match fmt.[j] with
          | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            add_char skip (add_conv skip i conv) 'i'
          | c -> add_conv skip i 'i' end
    | '{' | '(' as conv -> add_conv skip i conv
    | '}' | ')' as conv -> add_conv skip i conv
    | conv -> bad_conversion fmt i conv in
  let lim = len - 1 in
  let rec loop i =
    if i < lim then
     if fmt.[i] = '%' then loop (scan_flags false (succ i)) else
     loop (succ i) in
  loop 0;;

(* Returns a string that summarizes the typing information that a given
   format string contains.
   It also checks the well-formedness of the format string.
   For instance, [summarize_format_type "A number %d\n"] is "%i". *)
let summarize_format_type fmt =
  let len = String.length fmt in
  let b = Buffer.create len in
  let add i c = Buffer.add_char b c; succ i in
  let add_char skip i c =
    if skip then succ i else add i c
  and add_conv skip i c =
    if skip then Buffer.add_string b "%_" else Buffer.add_char b '%';
    add i c in
  iter_format_args fmt add_conv add_char;
  Buffer.contents b;;

(* Computes the number of arguments of a format (including flag
   arguments if any). *)
let nargs_of_format_type fmt =
  let num_args = ref 0
  and skip_args = ref 0 in
  let add_conv skip i c =
    let incr_args n = if c = 'a' then n := !n + 2 else n := !n + 1 in
    if skip then incr_args skip_args else incr_args num_args;
    succ i
  and add_char skip i c = succ i in
  iter_format_args fmt add_conv add_char;
  !skip_args + !num_args;;

let list_iter_i f l =
  let rec loop i = function
  | [] -> ()
  | x :: xs -> f i x; loop (succ i) xs in
  loop 0 l;;

(* Abstracting version of kprintf: returns a (curried) function that
   will print when totally applied. *)
let kapr kpr fmt =

  let nargs = nargs_of_format_type fmt in

  match nargs with
  | 0 -> kpr fmt [||]
  | 1 -> Obj.magic (fun x -> kpr fmt [|x|])
  | 2 -> Obj.magic (fun x y -> kpr fmt [|x; y|])
  | 3 -> Obj.magic (fun x y z -> kpr fmt [|x; y; z|])
  | 4 -> Obj.magic (fun x y z t -> kpr fmt [|x; y; z; t|])
  | 5 -> Obj.magic (fun x y z t u -> kpr fmt [|x; y; z; t; u|])
  | 6 -> Obj.magic (fun x y z t u v -> kpr fmt [|x; y; z; t; u; v|])
  | nargs ->
    let rec loop i args =
      if i >= nargs then
        let v = Array.make nargs (Obj.repr 0) in
        list_iter_i (fun i arg -> v.(nargs - i - 1) <- arg) args;
        kpr fmt v
      else Obj.magic (fun x -> loop (succ i) (x :: args)) in
    loop 0 [];;

(* Decode a %format and act on it.
   [fmt] is the printf format style, and [pos] points to a [%] character.
   After consuming the appropriate number of arguments and formatting
   them, one of the five continuations is called:
   [cont_s] for outputting a string (args: string, next pos)
   [cont_a] for performing a %a action (args: fn, arg, next pos)
   [cont_t] for performing a %t action (args: fn, next pos)
   [cont_f] for performing a flush action 
   [cont_m] for performing a %( action (args: sfmt, next pos)
   "next pos" is the position in [fmt] of the first character following
   the %format in [fmt]. *)

(* Note: here, rather than test explicitly against [String.length fmt]
   to detect the end of the format, we use [String.unsafe_get] and
   rely on the fact that we'll get a "nul" character if we access
   one past the end of the string.  These "nul" characters are then
   caught by the [_ -> bad_conversion] clauses below.
   Don't do this at home, kids. *)

let scan_format fmt args n pos cont_s cont_a cont_t cont_f cont_m =

  let get_arg args n = Obj.magic args.(int_of_sz n) in

  let rec scan_flags n widths i =
    match String.unsafe_get fmt i with
    | '*' ->
      let (width : int) = get_arg args n in
      scan_flags (succs n) (width :: widths) (succ i)
    | '0'..'9' | '.' | '#' | '-' | ' ' | '+' -> scan_flags n widths (succ i)
    | _ -> scan_conv n widths i
  and scan_conv n widths i =
    match String.unsafe_get fmt i with
    | '%' ->
      cont_s n "%" (succ i)
    | 's' | 'S' as conv ->
      let (x : string) = get_arg args n in
      let x = if conv = 's' then x else "\"" ^ String.escaped x ^ "\"" in
      let s =
        (* optimize for common case %s *)
        if i = succ pos then x else
        format_string (extract_format fmt pos i widths) x in
      cont_s (succs n) s (succ i)
    | 'c' | 'C' as conv ->
      let (x : char) = get_arg args n in
      let s =
        if conv = 'c' then String.make 1 x else "'" ^ Char.escaped x ^ "'" in
      cont_s (succs n) s (succ i)
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' as conv ->
      let (x : int) = get_arg args n in
      let s = format_int_with_conv conv (extract_format fmt pos i widths) x in
      cont_s (succs n) s (succ i)
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' as conv ->
      let (x : float) = get_arg args n in
      let s =
        if conv = 'F' then string_of_float x else
        format_float (extract_format fmt pos i widths) x in
      cont_s (succs n) s (succ i)
    | 'B' | 'b' ->
      let (x : bool) = get_arg args n in
      cont_s (succs n) (string_of_bool x) (succ i)
    | 'a' ->
      let printer = get_arg args n in
      let n = succs n in
      let arg = get_arg args n in
      cont_a (succs n) printer arg (succ i)
    | 't' ->
      let printer = get_arg args n in
      cont_t (succs n) printer (succ i)
    | 'l' | 'n' | 'L' as conv ->
      begin match String.unsafe_get fmt (succ i) with
      | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
        let s =
          match conv with
          | 'l' ->
            let (x : int32) = get_arg args n in
            format_int32 (extract_format fmt pos (succ i) widths) x
          | 'n' ->
            let (x : nativeint) = get_arg args n in
            format_nativeint (extract_format fmt pos (succ i) widths) x
          | _ ->
            let (x : int64) = get_arg args n in
            format_int64 (extract_format fmt pos (succ i) widths) x in
        cont_s (succs n) s (i + 2)
      | _ ->
        let (x : int) = get_arg args n in
        cont_s
          (succs n)
          (format_int_with_conv 'n' (extract_format fmt pos i widths) x)
          (succ i)
      end
    | '!' -> cont_f n (succ i)
    | '{' | '(' as conv (* ')' '}' *)->
      let (xf : ('a, 'b, 'c, 'd) format4) = get_arg args n in
      let i = succ i in
      let j = sub_format_for_printf conv fmt i + 1 in
      if conv = '{' (* '}' *) then
        (* Just print the format argument as a specification. *)
        cont_s (succs n) (summarize_format_type (format_to_string xf)) j else
        (* Use the format argument instead of the format specification. *)
        cont_m (succs n) xf j
    | ')' ->
      cont_s n "" (succ i)
    | conv ->
      bad_conversion fmt i conv in

  scan_flags n [] (succ pos);;

let mkprintf str get_out outc outs flush =
  let rec kprintf k fmt =
    let fmt = format_to_string fmt in
    let len = String.length fmt in

    let kpr fmt v =
      let out = get_out fmt in
      let rec doprn n i =
        if i >= len then Obj.magic (k out) else
        match String.unsafe_get fmt i with
        | '%' -> scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
        |  c  -> outc out c; doprn n (succ i)
      and cont_s n s i =
        outs out s; doprn n i
      and cont_a n printer arg i =
        if str then
          outs out ((Obj.magic printer : unit -> _ -> string) () arg)
        else
          printer out arg;
        doprn n i
      and cont_t n printer i =
        if str then
          outs out ((Obj.magic printer : unit -> string) ())
        else
          printer out;
        doprn n i
      and cont_f n i =
        flush out; doprn n i
      and cont_m n sfmt i =
        kprintf (Obj.magic (fun _ -> doprn n i)) sfmt in

      doprn (sz_of_int 0) 0 in

    kapr kpr fmt in

  kprintf;;

let kfprintf k oc =
  mkprintf false (fun _ -> oc) output_char output_string flush k
let fprintf oc = kfprintf ignore oc
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let kbprintf k b =
  mkprintf false (fun _ -> b) Buffer.add_char Buffer.add_string ignore k
let bprintf b = kbprintf ignore b

let get_buff fmt =
  let len = 2 * String.length fmt in
  Buffer.create len;;

let get_contents b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s;;

let get_cont k b = k (get_contents b);;

let ksprintf k =
  mkprintf true get_buff Buffer.add_char Buffer.add_string ignore (get_cont k);;

let kprintf = ksprintf;;

let sprintf fmt = ksprintf (fun s -> s) fmt;;
