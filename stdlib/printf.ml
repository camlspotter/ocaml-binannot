(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
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
       | '%' -> sub_sub (j + 1)
       | _ -> sub (j + 1)
    and sub_sub j =
       if j >= len then incomplete_format fmt else
       match fmt.[j] with
       | '(' | '{' as c ->
         let j = sub_fmt c (j + 1) in sub (j + 1)
       | ')' | '}' as c ->
         if c = close then j else bad_conversion fmt i c
       | _ -> sub (j + 1) in
    sub i in
  sub_fmt conv i;;

let sub_format_for_printf = sub_format incomplete_format bad_conversion;;

(* Returns a string that summarizes the typing information that a given
   format string contains.
   It also checks the well-formedness of the format string.
   For instance, [summarize_format_type "A number %d\n"] is "%i". *)
let summarize_format_type fmt =
  let len = String.length fmt in
  let b = Buffer.create len in
  let add i c = Buffer.add_char b c; i + 1 in
  let add_conv i c = Buffer.add_char b '%'; add i c in
  let rec scan_flags i =
    if i >= len then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '*' -> scan_flags (add_conv i '*')
    | '#' | '-' | ' ' | '+' -> scan_flags (succ i)
    | '_' -> Buffer.add_char b '_'; scan_flags (i + 1)
    | '0'..'9'
    | '.'  -> scan_flags (succ i)
    | _ -> scan_conv i
  and scan_conv i =
    if i >= len then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '%' | '!' -> succ i
    | 's' | 'S' | '[' -> add_conv i 's'
    | 'c' | 'C' -> add i 'c'
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' -> add_conv i 'i'
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> add_conv i 'f'
    | 'B' | 'b' -> add_conv i 'B'
    | 'a' | 't' as conv -> add_conv i conv
    | 'l' | 'n' | 'L' as conv ->
        let j = i + 1 in
        if j >= len then add_conv i 'i' else begin
          match fmt.[j] with
          | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' -> add (add_conv i conv) 'i'
          | c -> add_conv i 'i' end
    | '{' | '(' as conv -> add_conv i conv
    | '}' | ')' as conv -> add_conv i conv
    | conv -> bad_conversion fmt i conv in
  let lim = len - 1 in
  let rec loop i =
    if i < lim then
     if fmt.[i] = '%' then loop (scan_flags (i + 1)) else
     loop (i + 1) in
  loop 0;
  Buffer.contents b;;

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

let scan_format fmt pos cont_s cont_a cont_t cont_f cont_m =
  let rec scan_flags widths i =
    match String.unsafe_get fmt i with
    | '*' ->
        Obj.magic(fun w -> scan_flags (w :: widths) (succ i))
    | '0'..'9' | '.' | '#' | '-' | ' ' | '+' -> scan_flags widths (succ i)
    | _ -> scan_conv widths i
  and scan_conv widths i =
    match String.unsafe_get fmt i with
    | '%' ->
        cont_s "%" (succ i)
    | 's' | 'S' as conv ->
        Obj.magic (fun (s : string) ->
          let s = if conv = 's' then s else "\"" ^ String.escaped s ^ "\"" in
          if i = succ pos (* optimize for common case %s *)
          then cont_s s (succ i)
          else cont_s (format_string (extract_format fmt pos i widths) s)
                      (succ i))
    | 'c' | 'C' as conv ->
        Obj.magic (fun (c : char) ->
          if conv = 'c'
          then cont_s (String.make 1 c) (succ i)
          else cont_s ("'" ^ Char.escaped c ^ "'") (succ i))
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' as conv ->
        Obj.magic (fun (n : int) ->
          cont_s
            (format_int_with_conv conv (extract_format fmt pos i widths) n)
            (succ i))
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' as conv ->
        Obj.magic (fun (f : float) ->
          let s =
            if conv = 'F' then string_of_float f else
            format_float (extract_format fmt pos i widths) f in
          cont_s s (succ i))
    | 'B' | 'b' ->
        Obj.magic (fun (b : bool) ->
          cont_s (string_of_bool b) (succ i))
    | 'a' ->
        Obj.magic (fun printer arg ->
          cont_a printer arg (succ i))
    | 't' ->
        Obj.magic (fun printer ->
          cont_t printer (succ i))
    | 'l' | 'n' | 'L' as conv ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            begin match conv with
            | 'l' ->
               Obj.magic (fun (n : int32) ->
                 cont_s
                   (format_int32 (extract_format fmt pos (succ i) widths) n)
                   (i + 2))
            | 'n' ->
               Obj.magic (fun (n : nativeint) ->
                 cont_s
                   (format_nativeint (extract_format fmt pos (succ i) widths) n)
                   (i + 2))
            | _ ->
               Obj.magic (fun (n : int64) ->
                 cont_s
                   (format_int64 (extract_format fmt pos (succ i) widths) n)
                   (i + 2))
            end
        | _ ->
           Obj.magic (fun (n : int) ->
              cont_s
                (format_int_with_conv 'n' (extract_format fmt pos i widths) n)
                (succ i))
        end
    | '!' ->
        Obj.magic (cont_f (succ i))
    | '{' | '(' as conv ->
        Obj.magic (fun xf ->
          let i = succ i in
          let j = sub_format_for_printf conv fmt i + 1 in
          if conv = '{' then
            (* Just print the format argument as a specification. *)
            cont_s (summarize_format_type (string_of_format xf)) j else
            (* Use the format argument instead of the format specification. *)
            cont_m xf j)
    | ')' ->
        Obj.magic (cont_s "" (succ i))
    | conv ->
        bad_conversion fmt i conv in
  scan_flags [] (pos + 1)

(* Application to [fprintf], etc.  See also [Format.*printf]. *)

let rec kfprintf k chan fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in

  let rec doprn i =
    if i >= len then Obj.magic (k chan) else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f cont_m
    | c -> output_char chan c; doprn (succ i)
  and cont_s s i =
    output_string chan s; doprn i
  and cont_a printer arg i =
    printer chan arg; doprn i
  and cont_t printer i =
    printer chan; doprn i
  and cont_f i =
    flush chan; doprn i
  and cont_m sfmt i =
    kfprintf (Obj.magic (fun _ -> doprn i)) chan sfmt in

  doprn 0

let fprintf chan fmt = kfprintf (fun _ -> ()) chan fmt

let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let rec ksprintf k fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  let dst = Buffer.create (len + 16) in
  let rec doprn i =
    if i >= len then begin
      let res = Buffer.contents dst in
      Buffer.clear dst; (* just in case ksprintf is partially applied *)
      Obj.magic (k res)
    end else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f cont_m
    | c -> Buffer.add_char dst c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dst s; doprn i
  and cont_a printer arg i =
    Buffer.add_string dst (printer () arg); doprn i
  and cont_t printer i =
    Buffer.add_string dst (printer ()); doprn i
  and cont_f i = doprn i
  and cont_m sfmt i =
    ksprintf (fun res -> Obj.magic (cont_s res i)) sfmt in

  doprn 0

let sprintf fmt = ksprintf (fun x -> x) fmt

let kprintf = ksprintf

let rec bprintf dst fmt =
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  let rec doprn i =
    if i >= len then Obj.magic () else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t cont_f cont_m
    | c -> Buffer.add_char dst c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dst s; doprn i
  and cont_a printer arg i =
    printer dst arg; doprn i
  and cont_t printer i =
    printer dst; doprn i
  and cont_f i = doprn i
  and cont_m sfmt i =
    bprintf dst sfmt; doprn i in

  doprn 0
