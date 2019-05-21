(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright (c) 1997-2019 INRIA.                                     *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Clement Franchini <Clement.Franchini@inria.fr>                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The data type definitions for precedences used by pretty-printers
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

let must_parenthesize encl ctx =
  match ctx, encl with
  | (Ck_toplevel | Ck_record_field |
     Ck_tuple_item | Ck_argument), Em_do_close -> true

  | (Ck_toplevel | Ck_record_field), Em_tuple -> false
  | (Ck_tuple_item | Ck_argument), Em_tuple -> true

  | (Ck_toplevel | Ck_record_field | Ck_tuple_item), Em_application -> false
  | Ck_argument, Em_application -> true

  | (Ck_toplevel | Ck_record_field |
     Ck_tuple_item | Ck_argument), Em_do_not_close -> false
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
