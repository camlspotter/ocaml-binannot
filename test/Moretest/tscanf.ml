open Scanf;;

(* Auxilliaries. *)
let all_tests_ok = ref true;;

let finish () =
  match !all_tests_ok with
  | true ->
      prerr_endline "\nAll tests succeeded."
  | _ ->
      prerr_endline "\n\n********* Test suit failed. ***********\n";;

at_exit finish;;

let test_num = ref (-1);;

let print_test_number () =
  print_int !test_num; print_string " "; flush stdout;;

let next_test () =
 incr test_num;
 print_test_number ();;

let print_test_fail () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf "\n********* Test number %i failed ***********\n"
    !test_num);;

let print_failure_test_fail () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf "\n********* Failure Test number %i incorrectly failed ***********\n"
    !test_num);;

let print_failure_test_succeed () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf "\n********* Failure Test number %i failed to fail ***********\n"
    !test_num);;

let test b =
 next_test ();
 if not b then print_test_fail ();;

(* Applies f to x and checks that the evaluation indeed
   raises an exception that verifies the predicate [pred]. *)
let test_raises_exc_p pred f x =
 next_test ();
 try
   let b = f x in
   print_failure_test_succeed ();
   false
 with
 | x ->
   pred x || (print_failure_test_fail (); false);;

(* Applies f to x and checks that the evaluation indeed
   raises some exception. *)
let test_raises_some_exc f = test_raises_exc_p (fun _ -> true) f;;
let test_raises_this_exc exc = test_raises_exc_p (fun x -> x = exc);;

(* Applies f to x and checks that the evaluation indeed
   raises exception Failure s. *)

let test_raises_this_failure s f x =
  test_raises_exc_p (fun x -> x = Failure s) f x;;

(* Applies f to x and checks that the evaluation indeed
   raises the exception Failure. *)
let test_raises_some_failure f x =
  test_raises_exc_p (function Failure _ -> true | _ -> false) f x;;

let failure_test f x s = test_raises_this_failure s f x;;
let any_failure_test = test_raises_some_failure;;

let scan_failure_test f x =
  test_raises_exc_p (function Scan_failure _ -> true | _ -> false) f x;;

(* The ``continuation'' that returns the scanned value. *)
let void x = x;;

(* Testing space scanning. *)
let test0 () =
 (sscanf "" "" void) 1 +
 (sscanf "" " " void) 2 +
 (sscanf " " " " void) 3 +
 (sscanf "\t" " " void) 4 +
 (sscanf "\n" " " void) 5 +
 (sscanf "\n\t 6" " %d" void)
;;
test (test0 () = 21);;

(* Testing integer scanning %i and %d. *)
let test1 () =
 sscanf "1" "%d" void +
 sscanf " 2" " %d" void +
 sscanf " -2" " %d" void +
 sscanf " +2" " %d" void +
 sscanf " 2a " " %da" void;;

test (test1 () = 5);;

let test2 () =
 sscanf "123" "%2i" void +
 sscanf "245" "%d" void +
 sscanf " 2a " " %1da" void;;

test (test2 () = 259);;

let test3 () =
 sscanf "0xff" "%3i" void +
 sscanf "0XEF" "%3i" void +
 sscanf "x=-245" " x = %d" void +
 sscanf " 2a " " %1da" void;;

test (test3 () = -214);;

(* Testing float scanning. *)
(* f style. *)
let test4 () =
  bscanf (Scanning.from_string "1")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "-1")
    "%f" (fun b0 -> b0 = -1.0) &&
  bscanf (Scanning.from_string "+1")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "1.")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string ".1")
    "%f" (fun b0 -> b0 = 0.1) &&
  bscanf (Scanning.from_string "-.1")
    "%f" (fun b0 -> b0 = -0.1) &&
  bscanf (Scanning.from_string "+.1")
    "%f" (fun b0 -> b0 = 0.1) &&
  bscanf (Scanning.from_string "+1.")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "-1.")
    "%f" (fun b0 -> b0 = -1.0) &&
  bscanf (Scanning.from_string "0 1. 1.3")
    "%f %f %f" (fun b0 b1 b2 -> b0 = 0.0 && b1 = 1.0 && b2 = 1.3) &&
  bscanf (Scanning.from_string "0.113")
    "%4f" (fun b0 -> b0 = 0.11) &&
  bscanf (Scanning.from_string "0.113")
    "%5f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "000.113")
    "%15f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "+000.113")
    "%15f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "-000.113")
    "%15f" (fun b0 -> b0 = -0.113);;
test (test4 ());;

(* e style. *)
let test5 () =
  bscanf (Scanning.from_string "1e1")
    "%e" (fun b -> b = 10.0) &&
  bscanf (Scanning.from_string "1e+1")
    "%e" (fun b -> b = 10.0) &&
  bscanf (Scanning.from_string "10e-1")
    "%e" (fun b -> b = 1.0) &&
  bscanf (Scanning.from_string "10.e-1")
    "%e" (fun b -> b = 1.0) &&
  bscanf (Scanning.from_string "1e1 1.e+1 1.3e-1")
    "%e %e %e" (fun b1 b2 b3 -> b1 = 10.0 && b2 = b1 && b3 = 0.13) &&

(* g style. *)
  bscanf (Scanning.from_string "1 1.1 0e+1 1.3e-1")
    "%g %g %g %g" (fun b1 b2 b3 b4 ->
                     b1 = 1.0 && b2 = 1.1 && b3 = 0.0 && b4 = 0.13);;

test (test5 ());;

(* Testing boolean scanning. *)
let test6 () =
  bscanf (Scanning.from_string "truetrue") "%B%B"
         (fun b1 b2 -> (b1, b2) = (true, true))  &&
  bscanf (Scanning.from_string "truefalse") "%B%B"
         (fun b1 b2 -> (b1, b2) = (true, false)) &&
  bscanf (Scanning.from_string "falsetrue") "%B%B"
         (fun b1 b2 -> (b1, b2) = (false, true)) &&
  bscanf (Scanning.from_string "falsefalse") "%B%B"
         (fun b1 b2 -> (b1, b2) = (false, false)) &&
  bscanf (Scanning.from_string "true false") "%B %B"
         (fun b1 b2 -> (b1, b2) = (true, false));;

test (test6 ());;

(* Testing char scanning. *)

let test7 () =
  bscanf (Scanning.from_string "'a' '\n' '\t' '\000' '\032'")
         "%C %C %C %C %C"
    (fun c1 c2 c3 c4 c5 ->
       c1 = 'a' && c2 = '\n' && c3 = '\t' && c4 = '\000' && c5 = '\032') &&

(* Here \n, \t, and \032 are skipped due to the space semantics of scanf. *)
  bscanf (Scanning.from_string "a \n \t \000 \032b")
         "%c %c %c "
    (fun c1 c2 c3 ->
       c1 = 'a' && c2 = '\000' && c3 = 'b');;

test (test7 ());;

let verify_read c =
  let s = Printf.sprintf "%C" c in
  let ib = Scanning.from_string s in
  assert (bscanf ib "%C" void = c);;

let verify_scan_Chars () =
  for i = 0 to 255 do verify_read (char_of_int i) done;;

let test8 () = verify_scan_Chars () = ();;

test (test8 ());;

(* Testing string scanning. *)

(* %S and %s styles. *)
let unit fmt s =
  let ib = Scanning.from_string (Printf.sprintf "%S" s) in
  Scanf.bscanf ib fmt void;;

let test_fmt fmt s = unit fmt s = s;;

let test_S = test_fmt "%S";;
let test9 () =
  test_S "poi" &&
  test_S "a\"b" &&
  test_S "a\nb" &&
  test_S "a\010b" &&
  test_S "a\\\n\
          b \\\n\
          c\010\\\n\
          b" &&
  test_S "a\\\n\
          \\\n\
          \\\n\
          b \\\n\
          c\010\\\n\
          b"
;;
test (test9 ());;

let unit_S = unit "%S";;

let test10 () =
  let res = sscanf "Une cha�ne: \"celle-ci\" et \"celle-l�\"!"
               "%s %s %S %s %S %s"
               (fun s1 s2 s3 s4 s5 s6 -> s1 ^ s2 ^ s3 ^ s4 ^ s5 ^ s6) in
  res = "Unecha�ne:celle-cietcelle-l�!";;

test (test10 ());;

(* %[] style *)
let test11 () =
 sscanf "Pierre	Weis	70" "%s %s %s"
  (fun prenom nom poids ->
     prenom = "Pierre" && nom = "Weis" && int_of_string poids = 70)
 &&
 sscanf "Jean-Luc	de L�age	68" "%[^	] %[^	] %d"
  (fun prenom nom poids ->
     prenom = "Jean-Luc" && nom = "de L�age" && poids = 68)
 &&
 sscanf "Daniel	de Rauglaudre	66" "%s@\t %s@\t %d"
  (fun prenom nom poids ->
     prenom = "Daniel" && nom = "de Rauglaudre" && poids = 66)
;;

let test110 () =
 sscanf "" " " (fun x -> x) "" = "" &&
 sscanf "" "%[^\n]" (fun x -> x) = "" &&
 sscanf "" "%[^\n] " (fun x -> x) = "";;

let test111 () =
 test_raises_this_exc End_of_file (sscanf "" "%[^\n]@\n") (fun x -> x);;

test (test11 () && test110 () && test111 ());;

(* Scanning lists. *)
let ib () = Scanning.from_string "[1;2;3;4; ]";;

(* Statically known lists can be scanned directly. *)
let f ib =
 bscanf ib " [" ();
 bscanf ib " %i ;" (fun i ->
 bscanf ib " %i ;" (fun j ->
 bscanf ib " %i ;" (fun k ->
 bscanf ib " %i ;" (fun l ->
 bscanf ib " ]" ();
 [i; j; k; l]))));;

let test12 () = f (ib ()) = [1; 2; 3; 4];;

test (test12 ());;

(* A general list scanner that always fails to succeed. *)
let rec scan_elems ib accu =
 try bscanf ib " %i ;" (fun i -> scan_elems ib (i :: accu))
 with _ -> accu;;

let g ib = bscanf ib "[ " (); List.rev (scan_elems ib []);;

let test13 () = g (ib ()) = [1; 2; 3; 4];;

test (test13 ());;

(* A general int list scanner. *)
let rec scan_int_list ib =
 bscanf ib "[ " ();
 let accu = scan_elems ib [] in
 bscanf ib " ]" ();
 List.rev accu;;

let test14 () = scan_int_list (ib ()) = [1; 2; 3; 4];;

test (test14 ());;

(* A general list scanner that always succeeds. *)
let rec scan_elems ib accu =
  bscanf ib " %i %c"
   (fun i -> function
    | ';' -> scan_elems ib (i :: accu)
    | ']' -> List.rev (i :: accu)
    | c -> failwith "scan_elems");;

let rec scan_int_list ib =
 bscanf ib "[ " ();
 scan_elems ib [];;

let test15 () =
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1; 2; 3; 4];;

test (test15 ());;

let rec scan_elems ib accu =
  try
  bscanf ib "%c %i"
   (fun c i ->
    match c with
    | ';' -> scan_elems ib (i :: accu)
    | ']' -> List.rev (i :: accu)
    | '[' when accu = [] -> scan_elems ib (i :: accu)
    | c -> prerr_endline (String.make 1 c); failwith "scan_elems")
  with
  | Scan_failure _ -> bscanf ib "]" (); accu
  | End_of_file -> accu;;

let scan_int_list ib = scan_elems ib [];;

let test16 () =
  scan_int_list (Scanning.from_string "[]") = List.rev [] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = List.rev [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = List.rev [1;2;3;4] &&
  (* Should fail but succeeds! *)
  scan_int_list (Scanning.from_string "[1;2;3;4") = List.rev [1;2;3;4]
;;

test (test16 ());;

let rec scan_elems ib accu =
  bscanf ib " %i%[]; \t\n\r]"
   (fun i s ->
    match s with
    | ";" -> scan_elems ib (i :: accu)
    | "]" -> List.rev (i :: accu)
    | s -> List.rev (i :: accu));;

let scan_int_list ib =
 bscanf ib " [" ();
 scan_elems ib [];;

let test17 () =
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = [1;2;3;4] &&
  (* Should fail but succeeds! *)
  scan_int_list (Scanning.from_string "[1;2;3;4 5]") = [1;2;3;4]
;;

test (test17 ());;

let rec scan_elems ib accu =
  bscanf ib " %c " (fun c ->
    match c with
    | '[' when accu = [] ->
        (* begginning of list: could find either
           - an int, if the list is not empty,
           - the char ], if the list is empt *)
        bscanf ib "%[]]"
         (function
          | "]" -> accu
          | _ ->
           bscanf ib " %i " (fun i ->
            scan_rest ib (i :: accu)))
    | _ -> failwith "scan_elems")

and scan_rest ib accu =
  bscanf ib " %c " (fun c ->
    match c with
    | ';' ->
        bscanf ib "%[]]"
         (function
          | "]" -> accu
          | _ ->
            bscanf ib " %i " (fun i ->
            scan_rest ib (i :: accu)))
    | ']' -> accu
    | _ -> failwith "scan_rest");;


let scan_int_list ib = List.rev (scan_elems ib []);;

let test18 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = [1;2;3;4]
;;

test (test18 ());;

(* Those properly fail *)

let test19 () =
 failure_test
   scan_int_list (Scanning.from_string "[1;2;3;4 5]")
   "scan_rest";;

(test19 ());;

let test20 () =
  scan_failure_test 
   scan_int_list (Scanning.from_string "[1;2;3;4; ; 5]");;

(test20 ());;

let test21 () =
 scan_failure_test
   scan_int_list (Scanning.from_string "[1;2;3;4;;");;

(test21 ());;

let rec scan_elems ib accu =
  bscanf ib "%1[];]" (function
  | "]" -> accu
  | ";" -> scan_rest ib accu
  | _ ->
     failwith
      (Printf.sprintf "scan_int_list" (*
        "scan_int_list: char %i waiting for ']' or ';' but found %c"
        (Scanning.char_count ib) (Scanning.peek_char ib)*)))

and scan_rest ib accu =
 bscanf ib "%[]]" (function
 | "]" -> accu
 | _ -> scan_elem ib accu)

and scan_elem ib accu =
  bscanf ib " %i " (fun i -> scan_elems ib (i :: accu));;

let scan_int_list ib =
 bscanf ib " [ " ();
 List.rev (scan_rest ib []);;

let test22 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4];;

test (test22 ());;

(* Should work and does not!
scan_int_list (Scanning.from_string "[1;2;3;4; ]");;
(* Should lead to a bad input error. *)
scan_int_list (Scanning.from_string "[1;2;3;4 5]");;
scan_int_list (Scanning.from_string "[1;2;3;4;;");;
scan_int_list (Scanning.from_string "[1;2;3;4; ; 5]");;
scan_int_list (Scanning.from_string "[1;2;3;4;; 23]");;
*)

let rec scan_elems ib accu =
 try bscanf ib " %i %1[;]" (fun i s ->
  if s = "" then i :: accu else scan_elems ib (i :: accu))
 with Scan_failure _ -> accu;;

(* The general int list scanner. *)
let rec scan_int_list ib =
 bscanf ib "[ " ();
 let accu = scan_elems ib [] in
 bscanf ib " ]" ();
 List.rev accu;;

(* The general HO list scanner. *)
let rec scan_elems ib scan_elem accu =
 try scan_elem ib (fun i s ->
  let accu = i :: accu in
  if s = "" then accu else scan_elems ib scan_elem accu)
 with Scan_failure _ -> accu;;

let scan_list scan_elem ib =
 bscanf ib "[ " ();
 let accu = scan_elems ib scan_elem [] in
 bscanf ib " ]" ();
 List.rev accu;;

(* Deriving particular list scanners from the HO list scanner. *)
let scan_int_elem ib = bscanf ib " %i %1[;]";;
let scan_int_list = scan_list scan_int_elem;;

let test23 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4]
;;

test (test23 ());;

let test24 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4 5]")
and test25 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4;;")
and test26 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4; ; 5]")
and test27 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4;; 23]");;

 (test24 ()) &&
 (test25 ()) &&
 (test26 ()) &&
 (test27 ());;

(* To scan a Caml string:
   the format is "\"%s@\"".
   A better way would be to add a %S (String.escaped)
   %C (Char.escaped) *)
let scan_string_elem ib = bscanf ib " \"%s@\" %1[;]";;
let scan_string_list = scan_list scan_string_elem;;

let scan_String_elem ib = bscanf ib " %S %1[;]";;
let scan_String_list = scan_list scan_String_elem;;

let test28 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[\"Le\"]") = ["Le"] &&
  scan_string_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&
  scan_string_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&

  scan_String_list (Scanning.from_string "[]") = [] &&
  scan_String_list (Scanning.from_string "[\"Le\"]") = ["Le"] &&
  scan_String_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&
  scan_String_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]") =
    ["Le"; "langage"; "Objective"; "Caml"]
;;

test (test28 ());;

(* The general HO list scanner with continuations. *)
let rec scan_elems ib scan_elem accu =
 scan_elem ib
  (fun i s ->
     let accu = i :: accu in
     if s = "" then accu else scan_elems ib scan_elem accu)
  (fun ib exc -> accu);;

let scan_list scan_elem ib =
 bscanf ib "[ " ();
 let accu = scan_elems ib scan_elem [] in
 bscanf ib " ]" ();
 List.rev accu;;

(* Deriving particular list scanners from the HO list scanner. *)
let scan_int_elem ib f ek = kscanf ib ek " %i %1[;]" f;;
let scan_int_list = scan_list scan_int_elem;;

let test29 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4]
;;

test (test29 ());;

let scan_string_elem ib f ek = kscanf ib ek " %S %1[;]" f;;
let scan_string_list = scan_list scan_string_elem;;

let test30 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"]
;;

test (test30 ());;

(* A generic scan_elem, *)
let scan_elem fmt ib f ek = kscanf ib ek fmt f;;

(* Derivation of list scanners from the generic polymorphic scanners. *)
let scan_int_list = scan_list (scan_elem " %i %1[;]");;
let scan_string_list = scan_list (scan_elem " %S %1[;]");;
let scan_bool_list = scan_list (scan_elem " %B %1[;]");;
let scan_char_list = scan_list (scan_elem " %C %1[;]");;
let scan_float_list = scan_list (scan_elem " %f %1[;]");;

let rec scan_elems ib scan_elem accu =
 scan_elem ib
  (fun i ->
     let accu = i :: accu in
     kscanf ib
      (fun ib exc -> accu)
      " %1[;]"
      (fun s -> if s = "" then accu else scan_elems ib scan_elem accu))
  (fun ib exc -> accu);;

let scan_list scan_elem ib =
 bscanf ib "[ " ();
 let accu = scan_elems ib scan_elem [] in
 bscanf ib " ]" ();
 List.rev accu;;

let scan_int_list = scan_list (scan_elem " %i");;
let scan_string_list = scan_list (scan_elem " %S");;
let scan_bool_list = scan_list (scan_elem " %B");;
let scan_char_list = scan_list (scan_elem " %C");;
let scan_float_list = scan_list (scan_elem " %f");;

let test31 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4]
;;

test (test31 ());;

let test32 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"]
;;

test (test32 ());;

(* Using kscanf only. *)
let rec scan_elems ib scan_elem accu =
  kscanf ib (fun ib exc -> accu)
   scan_elem
   (fun i ->
      let accu = i :: accu in
      kscanf ib (fun ib exc -> accu)
       " %1[;] "
       (fun s -> if s = "" then accu else scan_elems ib scan_elem accu))
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem [] in
  bscanf ib " ]" ();
  List.rev accu
;;

let scan_int_list = scan_list "%i";;
let scan_string_list = scan_list "%S";;
let scan_bool_list = scan_list "%B";;
let scan_char_list = scan_list "%C";;
let scan_float_list = scan_list "%f";;

let test33 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[ 1 ]") = [1] &&
  scan_int_list (Scanning.from_string "[ 1 ; 2 ; 3 ; 4 ]") = [1; 2; 3; 4] &&
  scan_int_list (Scanning.from_string "[1 ;2 ;3 ;4;]") = [1; 2; 3; 4]
;;

test (test33 ());;

let test34 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"]
;;

test (test34 ());;

(* Testing the %N format. *)
let test35 () =
  sscanf "" "%N" (fun x -> x) = 0 &&
  sscanf "456" "%N" (fun x -> x) = 0 &&
  sscanf "456" "%d%N" (fun x y -> x, y) = (456, 1) &&
  sscanf " " "%N%s%N" (fun x s y -> x, s, y) = (0, "", 1)
;;

test (test35 ());;

(* Testing the %n format. *)
let test36 () =
  sscanf "" "%n" (fun x -> x) = 0 &&
  sscanf "456" "%n" (fun x -> x) = 0 &&
  sscanf "456" "%d%n" (fun x y -> x, y) = (456, 3) &&
  sscanf " " "%n%s%n" (fun x s y -> x, s, y) = (0, "", 1)
;;

test (test36 ());;

(* Weird tests to empty strings or formats. *)
let test37 () =
  sscanf "" "" true &&
  sscanf "" "" (fun x -> x) 1 = 1 &&
  sscanf "123" "" (fun x -> x) 1 = 1
;;

test (test37 ());;

(*******

print_string "Test number is "; 
print_int !test_num; print_string ". It should be 36."; 
print_newline();;

To be continued.

let digest () =
  let scan_line f = Scanf.scanf "%[^\n\r]@\n" f in
  let digest s = String.uppercase (Digest.to_hex (Digest.string s)) in
  let digest_line s = print_endline (s ^ "#" ^ digest s) in
  try
   while true do scan_line digest_line done
  with End_of_file -> ()
;;

let test37 () = ();;

test (test37 ());;

(* Trying to scan records. *)
let rec scan_fields ib scan_field accu =
  kscanf ib (fun ib exc -> accu)
   scan_field
   (fun i ->
      let accu = i :: accu in
      kscanf ib (fun ib exc -> accu)
       " %1[;] "
       (fun s -> if s = "" then accu else scan_fields ib scan_field accu))
;;

let scan_record scan_field ib =
  bscanf ib "{ " ();
  let accu = scan_fields ib scan_field [] in
  bscanf ib " }" ();
  List.rev accu
;;
***********)
