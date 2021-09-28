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

(** {3 OCamlGen precedences} *)

(**
  The data type definitions for precedences used by pretty-printers
  provided by and generated with OCamlGen. *)

type enclosing_mode =
  | Em_do_close
  | Em_tuple
  | Em_application
  | Em_do_not_close
;;

type context_kind =
  | Ck_toplevel
  | Ck_record_field
  | Ck_tuple_item
  | Ck_argument
;;

let use_delimiters enclosing_mode =
  match enclosing_mode with
  | Em_do_close -> (fun _ctx -> true)
  | Em_do_not_close -> (fun _ctx -> false)
  | Em_tuple ->
    (function
     | Ck_toplevel | Ck_record_field -> false
     | Ck_tuple_item | Ck_argument -> true)
  | Em_application ->
    (function
     | Ck_toplevel | Ck_record_field | Ck_tuple_item -> false
     | Ck_argument -> true)
;;

let string_of_enclosing_mode = function
  | Em_do_close -> "Em_do_close"
  | Em_tuple -> "Em_tuple"
  | Em_application -> "Em_application"
  | Em_do_not_close -> "Em_do_not_close"
;;

let string_of_context_kind = function
  | Ck_toplevel -> "Ck_toplevel"
  | Ck_record_field -> "Ck_record_field"
  | Ck_tuple_item -> "Ck_tuple_item"
  | Ck_argument -> "Ck_argument"
;;
