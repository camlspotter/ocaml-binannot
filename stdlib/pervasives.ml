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

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "power_float" "pow" "float"
external exp : float -> float = "exp_float" "exp" "float"
external acos : float -> float = "acos_float" "acos" "float"
external asin : float -> float = "asin_float" "asin" "float"
external atan : float -> float = "atan_float" "atan" "float"
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"
external cos : float -> float = "cos_float" "cos" "float"
external cosh : float -> float = "cosh_float" "cosh" "float"
external log : float -> float = "log_float" "log" "float"
external log10 : float -> float = "log10_float" "log10" "float"
external sin : float -> float = "sin_float" "sin" "float"
external sinh : float -> float = "sinh_float" "sinh" "float"
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
external tan : float -> float = "tan_float" "tan" "float"
external tanh : float -> float = "tanh_float" "tanh" "float"
external ceil : float -> float = "ceil_float" "ceil" "float"
external floor : float -> float = "floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "fmod_float" "fmod" "float"
external frexp : float -> float * int = "frexp_float"
external ldexp : float -> int -> float = "ldexp_float"
external modf : float -> float * float = "modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bytes : string -> float = "float_of_bytes"
let infinity =
  float_of_bytes "\127\240\000\000\000\000\000\000"
              (* 0x7F F0 00 00 00 00 00 00 *)
let neg_infinity =
  float_of_bytes "\255\240\000\000\000\000\000\000"
              (* 0xFF F0 00 00 00 00 00 00 *)
let nan =
  float_of_bytes "\127\240\000\000\000\000\000\001"
              (* 0x7F F0 00 00 00 00 00 01 *)
let max_float =
  float_of_bytes "\127\239\255\255\255\255\255\255"
              (* 0x7f ef ff ff ff ff ff ff *)
let min_float =
  float_of_bytes "\000\016\000\000\000\000\000\000"
              (* 0x00 10 00 00 00 00 00 00 *)
let epsilon_float =
  float_of_bytes "\060\176\000\000\000\000\000\000"
              (* 0x3c b0 00 00 00 00 00 00 *)
type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float -> fpclass = "classify_float"

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"
external string_create: int -> string = "create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string" "noalloc"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "int_of_string"

module String = struct
  external get : string -> int -> char = "%string_safe_get"
end

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    if s.[i] = '.' || s.[i] = 'e' then s
    else loop (i+1)
  in
  loop 0
;;

let string_of_float f = valid_float_lexem (format_float "%.17g" f);;

external float_of_string : string -> float = "float_of_string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

external open_descriptor_out: int -> out_channel = "caml_open_descriptor_out"
external open_descriptor_in: int -> in_channel = "caml_open_descriptor_in"

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc: string -> open_flag list -> int -> int = "sys_open"

let open_out_gen mode perm name =
  open_descriptor_out(open_desc name mode perm)

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush : out_channel -> unit = "caml_flush"

external out_channels_list : unit -> out_channel list 
                           = "caml_out_channels_list"

let flush_all () = 
  let rec iter = function
      [] -> ()
    | a::l -> (try flush a with _ -> ()); iter l
  in iter (out_channels_list ())

external unsafe_output : out_channel -> string -> int -> int -> unit
                       = "caml_output"

external output_char : out_channel -> char -> unit = "caml_output_char"

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

external output_byte : out_channel -> int -> unit = "caml_output_char"
external output_binary_int : out_channel -> int -> unit = "caml_output_int"

external marshal_to_channel : out_channel -> 'a -> unit list -> unit
     = "output_value"
let output_value chan v = marshal_to_channel chan v []

external seek_out : out_channel -> int -> unit = "caml_seek_out"
external pos_out : out_channel -> int = "caml_pos_out"
external out_channel_length : out_channel -> int = "caml_channel_size"
external close_out_channel : out_channel -> unit = "caml_close_channel"
let close_out oc = flush oc; close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
external set_binary_mode_out : out_channel -> bool -> unit
                             = "caml_set_binary_mode"

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in(open_desc name mode perm)

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char : in_channel -> char = "caml_input_char"

external unsafe_input : in_channel -> string -> int -> int -> int 
                      = "caml_input"

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

external input_scan_line : in_channel -> int = "caml_input_scan_line"

let input_line chan =
  let rec build_result buf pos = function
    [] -> buf
  | hd :: tl ->
      let len = string_length hd in
      string_blit hd 0 buf (pos - len) len;
      build_result buf (pos - len) tl in
  let rec scan accu len =
    let n = input_scan_line chan in
    if n = 0 then begin                   (* n = 0: we are at EOF *)
      match accu with
        [] -> raise End_of_file
      | _  -> build_result (string_create len) len accu
    end else if n > 0 then begin          (* n > 0: newline found in buffer *)
      let res = string_create (n-1) in
      ignore (unsafe_input chan res 0 (n-1));
      ignore (input_char chan);           (* skip the newline *)
      match accu with
        [] -> res
      |  _ -> let len = len + n - 1 in 
              build_result (string_create len) len (res :: accu)
    end else begin                        (* n < 0: newline not found *)
      let beg = string_create (-n) in
      ignore(unsafe_input chan beg 0 (-n));
      scan (beg :: accu) (len - n)
    end
  in scan [] 0

external input_byte : in_channel -> int = "caml_input_char"
external input_binary_int : in_channel -> int = "caml_input_int"
external input_value : in_channel -> 'a = "input_value"
external seek_in : in_channel -> int -> unit = "caml_seek_in"
external pos_in : in_channel -> int = "caml_pos_in"
external in_channel_length : in_channel -> int = "caml_channel_size"
external close_in : in_channel -> unit = "caml_close_channel"
let close_in_noerr ic = (try close_in ic with _ -> ());;
external set_binary_mode_in : in_channel -> bool -> unit
                            = "caml_set_binary_mode"

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s = output_string stdout s; output_char stdout '\n'
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Operations on large files *)

module LargeFile =
  struct
    external seek_out : out_channel -> int64 -> unit = "caml_seek_out_64"
    external pos_out : out_channel -> int64 = "caml_pos_out_64"
    external out_channel_length : out_channel -> int64 = "caml_channel_size_64"
    external seek_in : in_channel -> int64 -> unit = "caml_seek_in_64"
    external pos_in : in_channel -> int64 = "caml_pos_in_64"
    external in_channel_length : in_channel -> int64 = "caml_channel_size_64"
  end

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Formats *)
external format_of_string :
 ('a, 'b, 'c, 'd) format -> ('a, 'b, 'c, 'd) format = "%identity"
external string_of_format : ('a, 'b, 'c, 'd) format -> string = "%identity"

external string_to_format : string -> ('a, 'b, 'c, 'd) format = "%identity"
let (( ^^ ) : ('a, 'b, 'c, 'd) format -> ('d, 'b, 'c, 'e) format ->
              ('a, 'b, 'c, 'e) format) = fun fmt1 fmt2 ->
  string_to_format (string_of_format fmt1 ^ string_of_format fmt2);;

(* Miscellaneous *)

external sys_exit : int -> 'a = "sys_exit"

let exit_function = ref flush_all

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

external register_named_value: string -> 'a -> unit = "register_named_value"

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
