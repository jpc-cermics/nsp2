(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright 1997-2015 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Runtime library for pretty-printers generated with OCamlGen.*)

open Format;;

(* Printing tokens that must be escaped.
   These escapes are not in the sense of String.escaped or
   Char.escaped, since accented chars should not be replaced by the
   decimal encoding of their ascii code. *)

(* Printing strings with quotes. *)
let escape_string s =
  let more = ref 0 in
  for i = 0 to String.length s - 1 do
   match s.[i] with
   | '\\' | '\"' | '\'' -> incr more
   |  _ -> ()
  done;
  if !more = 0 then s else
  let res = String.create (String.length s + !more) in
  let j = ref 0 in
  for i = 0 to String.length s - 1 do
   let c = s.[i] in
   match c with
   | '\\' | '\"' | '\'' ->
     res.[!j] <- '\\'; incr j;
     res.[!j] <- c; incr j
   | _ ->
     res.[!j] <- c; incr j
  done;
  res
;;

let escape_char c =
  if c = '\\' then "\\\\" else
  if c = '\'' then "\\\'" else
  if c = '\"' then "\\\"" else
  String.make 1 c
;;

let print_quoted_string ppf s =
  fprintf ppf "\"%s\"" (escape_string s)
;;
let print_quoted_char ppf c =
  fprintf ppf "'%s'" (escape_char c)
;;
let print_quoted_int ppf i =
  if i < 0 then fprintf ppf "(%i)" i else fprintf ppf "%i" i
;;
let print_quoted_int32 ppf i =
  if i < 0l then fprintf ppf "(%li)" i else fprintf ppf "%li" i
;;
let print_quoted_int64 ppf i =
  if i < 0L then fprintf ppf "(%Li)" i else fprintf ppf "%Li" i
;;
let print_quoted_nativeint ppf i =
  if i < 0n then fprintf ppf "(%ni)" i else fprintf ppf "%ni" i
;;
let print_quoted_float ppf f =
  if f <= 0.0 then fprintf ppf "(%f)" f else fprintf ppf "%f" f
;;

(* Iterators *)
let print_Couple pp_a pp_b ppf (a, b) =
  Format.fprintf ppf "@[<1>(%a,@ %a)@]" pp_a a pp_b b
;;

let print_Triple pp_a pp_b pp_c ppf (a, b, c) =
  Format.fprintf ppf "@[<1>(%a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c
;;

let print_List f ppf l =
  let pl ppf l =
    List.iter (fun x -> fprintf ppf "%a;@ " f x) l in
  fprintf ppf "@[<2>[@ %a@,]@]" pl l
;;

let print_Array f ppf v =
  let pl ppf v =
    let l = Array.length v in
    for i = 0 to l - 1 do
      fprintf ppf "%a;@ " f v.(i)
    done in
  fprintf ppf "@[<3>[|@ %a@,|]@]" pl v
;;

let print_Option f ppf = function
  | None -> fprintf ppf "%s" "None"
  | Some x -> fprintf ppf "@[<2>Some@ %a@]" f x
;;

let print_Ref f ppf x = fprintf ppf "@[<2>ref@ %a@]" f !x
;;

let print_couple fmt pp_a pp_b ppf (a, b) =
  Format.fprintf ppf "%a" pp_a a;
  Format.fprintf ppf fmt;
  Format.fprintf ppf "%a" pp_b b
;;

let print_triple fmt pp_a pp_b pp_c ppf (a, b, c) =
  Format.fprintf ppf "%a" pp_a a;
  Format.fprintf ppf fmt;
  Format.fprintf ppf "%a" pp_b b;
  Format.fprintf ppf fmt;
  Format.fprintf ppf "%a" pp_c c
;;

let print_list fmt f =
  let rec loop ppf = function
    | [] -> ()
    | x :: l ->
      f ppf x;
      if l <> [] then
      begin Format.fprintf ppf fmt; loop ppf l end in
  loop
;;

let print_array fmt f ppf v =
  let rec loop ppf i j =
    match compare j 1 with
    | 0 -> f ppf v.(i)
    | 1 -> f ppf v.(i); Format.fprintf ppf fmt; loop ppf (i + 1) (j - 1)
    | _ -> () in
  loop ppf 0 (Array.length v)
;;

let print_option fmt f ppf = function
  | None -> ()
  | Some x ->
    Format.fprintf ppf fmt;
    f ppf x
;;

let print_ref fmt f ppf x =
  Format.fprintf ppf fmt;
  f ppf !x
;;

let print_unit ppf () = fprintf ppf "%s" "()"
;;

let print_bool ppf = function
  | true -> fprintf ppf "%s" "true"
  | _ -> fprintf ppf "%s" "false"
;;

let print_poly ppf _ =
  fprintf ppf "%s" "<poly>"
;;

let print_abstract ppf _ = fprintf ppf "%s" "<abstract>"
;;

let print_external = print_abstract;;

let print_fun _p_a _p_b ppf _f =
  fprintf ppf "%s" "<fun>"
;;

(******************************

let print_list fmt f ppf l =
  let fmt = fmt ^^ "%a" in
  let rec loop ppf = function
    | [] -> ()
    | x :: l ->
      f ppf x;
      if l <> [] then Format.fprintf ppf fmt loop l in
    loop ppf l
;;

val print_list :
  ('a eta_printer, Format.formatter, unit, 'b eta_printer) format4 ->
  ('a, 'a list) printer_lifter
;;
let print_list fmt f ppf l =
  match l with
  | [] -> ()
  | [ x; ] -> f ppf x
  | x :: l ->
    let fmt = fmt ^^ "%a" in
    let rec loop l ppf x =
      f ppf x;
      match l with
      | [] -> ()
      | x :: l ->
        Format.fprintf ppf fmt (loop l) x in
    loop l ppf x
;;

(*let print_array fmt f ppf v =
  let lim = Array.length v in
  if lim > 0 then
  let lim = lim - 1 in
  let fmt = fmt ^^ "%a" in
  let rec loop ppf i =
    (f ppf v.(i) : unit);
    if i < lim then Format.fprintf ppf fmt loop (i + 1) in
  loop ppf 0
;;*)

val print_array :
  ('a eta_printer, Format.formatter, unit, 'b eta_printer) format4 ->
  ('a, 'a array) printer_lifter
;;
let print_array fmt f ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> f ppf  v.(0)
  | lim ->
    let fmt = fmt ^^ "%a" in
    let rec loop i ppf x =
      f ppf x;
      let i = i + 1 in
      if i < lim then Format.fprintf ppf fmt (loop i) v.(i) in
    loop 0 ppf v.(0)
;;

val print_option :
  ('a eta_printer, Format.formatter, unit, 'b eta_printer) format4 ->
  ('a, 'a option) printer_lifter
;;
let print_option fmt f ppf opt =
  let fmt = fmt ^^ "%a" in
  match opt with
  | None -> ()
  | Some x ->
    Format.fprintf ppf fmt f x
;;
*****************************************)

(*****************************************
let print_array fmt f ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> f ppf  v.(0)
  | lim ->
    let rec loop ppf i =
      f ppf v.(i);
      let i = i + 1 in
      if i < lim then
       begin Format.fprintf ppf fmt; loop ppf i end in
    loop ppf 0
;;

let print_array fmt f ppf v =
  let len = Array.length v in
  let rec loop ppf = function
    | 0 -> ()
    | i ->
      f ppf v.(len - i);
      let i = i - 1 in
      if i > 0 then
        begin Format.fprintf ppf fmt; loop ppf i end in
  loop ppf len
;;

let print_array fmt f ppf v =
  let len = Array.length v in
  let rec loop ppf = function
    | 0 -> ()
    | i ->
      f ppf v.(len - i);
      if i > 1 then
        begin Format.fprintf ppf fmt; loop ppf (i - 1) end in
  loop ppf len
;;

let print_array fmt f ppf v =
  let len = Array.length v in
  let rec loop ppf i =
    match compare i 1 with
    | 0 -> f ppf v.(len - i)
    | 1 -> f ppf v.(len - i); Format.fprintf ppf fmt; loop ppf (i - 1)
    | _ -> () in
  loop ppf len
;;

(* To test:

let print_vect ppf v =
  Format.fprintf ppf
   "%a@."
   (print_array ";" (fun ppf -> Format.fprintf ppf "%i"))
   v
;;

let print_v = print_vect Format.std_formatter;;

print_v [||];;

print_v [|1|];;

print_v [|1;2|];;

print_v [|1;2;3|];;

*)

*********************************************)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
