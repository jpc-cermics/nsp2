(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*                       EPI Serena, INRIA Paris                       *)
(*                                                                     *)
(*  Copyright (c) 1997-2018 INRIA.                                     *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Clement Franchini <Clement.Franchini@inria.fr>                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Code generated for OCaml compiler with version >= 4.02. *)

  let bytes_to_string = Bytes.unsafe_to_string;;

(* End of generated code. *)

(* Runtime library for pretty-printers generated with OCamlGen. *)

(* Printing tokens that must be escaped.
   These escapes are not in the sense of String.escaped or
   Char.escaped, since accented chars should not be replaced by the
   decimal encoding of their ascii code. *)

let escape_char c =
  if c = '\\' then "\\\\" else
  if c = '\'' then "\\\'" else
  if c = '\"' then "\\\"" else
  String.make 1 c
;;

let escape_string s =
  let len = String.length s in
  let more = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\\' | '\"' | '\'' -> incr more
    |  _ -> ()
  done;
  if !more = 0 then s else
  let res = Buffer.create (len + !more) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    match c with
    | '\\' | '\"' | '\'' ->
      Buffer.add_char res '\\';
      Buffer.add_char res c
    | _ ->
      Buffer.add_char res c
  done;
  Buffer.contents res
;;

let break_back ppf n =
  Format.pp_print_break ppf n (-2)
;;

let space_parens_print ppf fmt x =
  Format.fprintf ppf " (@,%a)"
    (fun ppf x ->
     Format.fprintf ppf fmt x;
     break_back ppf 0)
    x
;;

open Og_precedence;;

(* Basic printers. *)

(* The Caml way to print basic values. *)

let printer_Unit ctx ppf () =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ ()"
  else Format.fprintf ppf "()"

and printer_Bool ctx ppf b =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %B" b
  else Format.fprintf ppf "%B" b

and printer_Char ctx ppf c =
  let s = escape_char c in
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ \'%s\'" s
  else Format.fprintf ppf "\'%s\'" s

and printer_String ctx ppf s =
  let s = escape_string s in
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ \"%s\"" s
  else Format.fprintf ppf "\"%s\"" s
;;

let printer_Int ctx ppf n =
  if ctx = Og_precedence.Ck_argument then
    if n >= 0
    then Format.fprintf ppf "@ %i" n
    else space_parens_print ppf "%i" n else
  if ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %i" n
  else Format.fprintf ppf "%i" n

and printer_Int32 ctx ppf n =
  if ctx = Og_precedence.Ck_argument then
    if n >= 0l
    then Format.fprintf ppf "@ %li" n
    else space_parens_print ppf "%li" n else
  if ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %li" n
  else Format.fprintf ppf "%li" n

and printer_Int64 ctx ppf n =
  if ctx = Og_precedence.Ck_argument then
    if n >= 0L
    then Format.fprintf ppf "@ %Li" n
    else space_parens_print ppf "%Li" n else
  if ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %Li" n
  else Format.fprintf ppf "%Li" n

and printer_Nativeint ctx ppf n =
  if ctx = Og_precedence.Ck_argument then
    if n >= 0n
    then Format.fprintf ppf "@ %ni" n
    else space_parens_print ppf "%ni" n else
  if ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %ni" n
  else Format.fprintf ppf "%ni" n

and printer_Float ctx ppf f =
  if ctx = Og_precedence.Ck_argument then
    if not (f < 0. || 1. /. f = neg_infinity)
    then Format.fprintf ppf "@ %f" f
    else space_parens_print ppf "%f" f else
  if ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %f" f
  else Format.fprintf ppf "%f" f
;;

let printer_Bytes ctx ppf bs = printer_String ctx ppf (bytes_to_string bs);;

let printer_Format6 _pp_a _pp_b _pp_c _pp_d _pp_e _pp_f ctx ppf fmt_s =
  printer_String ctx ppf (string_of_format fmt_s)
;;

let printer_Format4 pp_a pp_b pp_c pp_d =
  printer_Format6 pp_a pp_b pp_c pp_c pp_c pp_d
;;

let printer_Format pp_a pp_b pp_c =
  printer_Format4 pp_a pp_b pp_c pp_c
;;

(* The user way to print basic values.
   Caml lexical convention may be violated. *)

let printer_unit = printer_Unit

and printer_bool = printer_Bool

(* We print the char 'as is' *)
and printer_char ctx ppf c =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %c" c
  else Format.fprintf ppf "%c" c

(* We print the string 'as is' *)
and printer_string ctx ppf s =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %s" s
  else Format.fprintf ppf "%s" s
;;

(* Print numbers 'as is' *)
let printer_int ctx ppf n =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %i" n
  else Format.fprintf ppf "%i" n

and printer_int32 ctx ppf n =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %li" n
  else Format.fprintf ppf "%li" n

and printer_int64 ctx ppf n =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %Li" n
  else Format.fprintf ppf "%Li" n

and printer_nativeint ctx ppf n =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %ni" n
  else Format.fprintf ppf "%ni" n

and printer_float ctx ppf f =
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "@ %f" f
  else Format.fprintf ppf "%f" f
;;

let printer_bytes ctx ppf bs = printer_string ctx ppf (bytes_to_string bs);;

(* Printing format strings *)
let printer_format6 _pp_a _pp_b _pp_c _pp_d _pp_e _pp_f ctx ppf fmt_s =
  printer_string ctx ppf (string_of_format fmt_s)
;;

let printer_format4 pp_a pp_b pp_c pp_d =
  printer_format6 pp_a pp_b pp_c pp_c pp_c pp_d
;;

let printer_format pp_a pp_b pp_c =
  printer_format4 pp_a pp_b pp_c pp_c
;;

(* Polymorphic printers. *)

(* The Caml way to print polymorphic values. *)

let printer_List pp_a ctx ppf l =
  let pp_a = pp_a Ck_toplevel in
  let rec print_body ppf = function
    | [] -> ()
    | [ elem ] -> Format.fprintf ppf "%a;" pp_a elem
    | elem :: elems ->
      Format.fprintf ppf "%a;@ %a" pp_a elem print_body elems in
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then
    match l with
    | [] -> Format.fprintf ppf "@ []"
    | l -> Format.fprintf ppf " [@ @[%a@]%a]" print_body l break_back 1
  else
    match l with
    | [] -> Format.fprintf ppf "[]"
    | l ->
      Format.fprintf ppf "@[<hv %d>[@ @[%a@]%a]@]"
        2
        print_body l
        break_back 1

and printer_Array pp_a ctx ppf a =
  let pp_a = pp_a Ck_toplevel in
  let lim = Array.length a in
  let rec print_body ppf i =
    if i = lim then () else
    let elem = a.(i) in
    if i = pred lim
    then Format.fprintf ppf "%a;" pp_a elem
    else Format.fprintf ppf "%a;@ %a" pp_a elem print_body (succ i) in
  if ctx = Og_precedence.Ck_argument
  || ctx = Og_precedence.Ck_record_field
  then
    if lim = 0
    then Format.fprintf ppf "@ [||]"
    else Format.fprintf ppf " [|@ @[%a@]%a|]" print_body 0 break_back 1
  else
    if lim = 0
    then Format.fprintf ppf "[||]"
    else
      Format.fprintf ppf "@[<hv %d>[|@ @[%a@]%a|]@]"
        2
        print_body 0
        break_back 1
;;

let printer_Option pp_a ctx ppf = function
  | None ->
    if ctx = Og_precedence.Ck_argument
    || ctx = Og_precedence.Ck_record_field
    then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,None"
  | Some a ->
    if ctx = Og_precedence.Ck_argument
    then Format.fprintf ppf "%,@ (@,"
    else
      if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Some%a@]"
      (pp_a Og_precedence.Ck_argument) a;
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)";

and printer_Ref pp_a ctx ppf = function
  | { contents = a; } ->
    if ctx = Og_precedence.Ck_argument
    then Format.fprintf ppf "%, (@,"
    else
      if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>ref%a@]"
      (pp_a Og_precedence.Ck_argument) a;
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)";

and printer_Lazy pp_a ctx ppf z =
  if not (Lazy.is_val z) then
    begin
      if ctx = Og_precedence.Ck_argument
      || ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
      Format.fprintf ppf "<lazy>";
    end
  else
    begin
      if ctx = Og_precedence.Ck_argument
      then Format.fprintf ppf "%, (@,"
      else
        if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
      Format.fprintf ppf "%,@[<hv 2>lazy%a@]"
        (pp_a Og_precedence.Ck_argument) (Lazy.force z);
      if ctx = Og_precedence.Ck_argument
      then Format.fprintf ppf "%,@;<0 -2>)";
    end
;;

(* The user way to print polymorphic values.
   Caml lexical convention may be violated. *)

let user_printer_list
  fmt_begin fmt_empty fmt_separator fmt_terminator fmt_end pp_a ctx ppf l =
  let pp_a = pp_a Ck_toplevel in
  let rec print_body ppf = function
    | [] -> ()
    | [ elem ] ->
      Format.fprintf ppf "%a%a"
        pp_a elem
        Format.fprintf fmt_terminator
    | elem :: elems ->
      Format.fprintf ppf "%a%a%a"
        pp_a elem
        Format.fprintf fmt_separator
        print_body elems in
  if ctx = Og_precedence.Ck_argument ||
     ctx = Og_precedence.Ck_record_field
  then
    match l with
    | [] ->
      Format.fprintf ppf "@ %a" Format.fprintf fmt_empty
    | l ->
      Format.fprintf ppf " %a[@ @[%a@]%a%a"
        Format.fprintf fmt_begin
        print_body l
        break_back 1
        Format.fprintf fmt_end
  else
    match l with
    | [] -> Format.fprintf ppf fmt_empty
    | l ->
      Format.fprintf ppf "@[<hv %d>%a@ @[%a@]%a%a@]"
        2
        Format.fprintf fmt_begin
        print_body l
        break_back 1
        Format.fprintf fmt_end

(*
Caml conventions for arrays are:
   fmt_begin = "[|"
   fmt_empty = "[||]"
   fmt_separator = ";@ "
   fmt_terminator = ";"
   fmt_end = "|]"
*)
and user_printer_array
  fmt_begin fmt_empty fmt_separator fmt_terminator fmt_end pp_a ctx ppf a =
  let pp_a = pp_a Ck_toplevel in
  let lim = Array.length a in
  let rec print_body ppf i =
    if i = lim then () else
    let elem = a.(i) in
    if i = pred lim
    then Format.fprintf ppf "%a%a"
           pp_a elem
           Format.fprintf fmt_terminator
    else Format.fprintf ppf "%a%a%a"
           pp_a elem
           Format.fprintf fmt_separator
           print_body (succ i) in
  if ctx = Og_precedence.Ck_argument ||
     ctx = Og_precedence.Ck_record_field
  then
    if lim = 0
    then Format.fprintf ppf "@ %a" Format.fprintf fmt_empty
    else Format.fprintf ppf " %a@ @[%a@]%a%a"
           Format.fprintf fmt_begin
           print_body 0 break_back 1
           Format.fprintf fmt_end
  else
    if lim = 0
    then Format.fprintf ppf fmt_empty
    else
      Format.fprintf ppf "@[<hv %d>%a@ @[%a@]%a%a@]"
        2
        Format.fprintf fmt_begin
        print_body 0
        break_back 1
        Format.fprintf fmt_end
;;

let printer_list pp_a = user_printer_list "" "" "@ " "" "" pp_a
and printer_array pp_a = user_printer_array "" "" "@ " "" "" pp_a
(*
   fmt_begin = "[|"
   fmt_empty = "[||]"
   fmt_separator = ";@ "
   fmt_terminator = ";"
   fmt_end = "|]"
*)

(*
  Caml convention for options are:
  fmt_none = "None"
  fmt_some = "Some"
*)
let user_printer_option fmt_none fmt_some pp_a ctx ppf = function
  | None ->
    if fmt_none <> format_of_string "" &&
       (ctx = Og_precedence.Ck_argument ||
        ctx = Og_precedence.Ck_record_field)
      then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf fmt_none
  | Some a ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>%a%a@]"
      Format.fprintf fmt_some
      (pp_a Og_precedence.Ck_argument) a;
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
;;

(*
  Caml convention for references are:
  fmt_ref = "ref"

  Other conventions are using a record with a mutable contents field.
  We do not use it here.
*)
let user_printer_ref fmt_ref pp_a ctx ppf = function
  | { contents = a; } ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>%a%a@]"
      Format.fprintf fmt_ref
      (pp_a Og_precedence.Ck_argument) a;
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
;;

(*
 Caml convention for lazy values are:
 fmt_thunk = "<lazy>"
 fmt_lazy = "lazy"
*)
let user_printer_lazy fmt_lazy fmt_thunk pp_a ctx ppf z =
  if not (Lazy.is_val z) then
    begin
      if ctx = Og_precedence.Ck_argument
      || ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
      Format.fprintf ppf fmt_thunk;
    end
  else
    begin
      if ctx = Og_precedence.Ck_argument
      then Format.fprintf ppf "%, (@,"
      else
        if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
      Format.fprintf ppf "%,@[<hv 2>%a%a@]"
        Format.fprintf fmt_lazy
        (pp_a Og_precedence.Ck_argument) (Lazy.force z);
      if ctx = Og_precedence.Ck_argument
      then Format.fprintf ppf "%,@;<0 -2>)";
    end
;;

let printer_option pp_a = user_printer_option "" "" pp_a
and printer_ref pp_a = user_printer_ref "" pp_a
and printer_lazy pp_a = user_printer_lazy "" "<*>" pp_a

(* Printers for functional, abstract, lazy, and polymorphic values. *)

(* The Caml way to print abstract values. *)

let printer_abstract_value label ctx ppf =
  if ctx = Og_precedence.Ck_argument ||
     ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "%,@ ";
  Format.fprintf ppf "<%s>" label
;;

let printer_Fun _pp_a _pp_b ctx ppf _f =
  printer_abstract_value "fun" ctx ppf
and printer_Poly ctx ppf _x =
  printer_abstract_value "poly" ctx ppf
and printer_Abstract ctx ppf _x =
  printer_abstract_value "abstract" ctx ppf
and printer_External ctx ppf _x =
  printer_abstract_value "external" ctx ppf
;;

(* The user way to print functional, abstract, lazy, and polymorphic values. *)

let user_printer_abstract_value fmt_abstract ctx ppf =
  if ctx = Og_precedence.Ck_argument ||
     ctx = Og_precedence.Ck_record_field
  then Format.fprintf ppf "%,@ ";
  Format.fprintf ppf fmt_abstract
;;

let printer_fun _pp_a _pp_b ctx ppf _x =
  user_printer_abstract_value "fun" ctx ppf
and printer_poly ctx ppf _x =
  user_printer_abstract_value  "poly" ctx ppf
and printer_abstract ctx ppf _x =
  user_printer_abstract_value "abstract" ctx ppf
and printer_external ctx ppf _x =
  user_printer_abstract_value "external" ctx ppf
;;
