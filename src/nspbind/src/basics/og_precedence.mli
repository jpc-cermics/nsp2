(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright (c) 1997-2015 INRIA.                                     *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Clement Franchini <Clement.Franchini@inria.fr>                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 OCamlGen precedences}

 The data type definitions for precedences used by pretty-printers or
 readers provided by and generated with OCamlGen.

 OCamlGen generates pretty-printers or readers for values of user-defined
 types.

 OCamlGen provides pretty-printers or readers for values of any predefined
 OCaml type:
 * values of basic monomorphic type such as bool, string or numbers;
 * values of basic polymorphic type such as couple, list, array, or format.

 Type [enclosing_mode] specifies the enclosing mode needed by pretty-printers
 or readers for each type.

 Type [context_kind] is used to specify the context in which pretty-printers
 or readers are used (both provided and generated).

 The comparison function [must_parenthesize] specifies the behavior of each
 [enclosing_mode] in each [context_kind] for OCaml syntax. *)

type enclosing_mode =
  | Em_do_close
    (** Item must be enclosed within matching ``delimitors''
      (i.e. matching parens for expressions, matching brackets for list,
       matching bracket bars for array, or matching braces for record). *)
  | Em_tuple
    (** Item is a tuple, it could be enclosed or not within parens according to
      the context. *)
  | Em_application
    (** Item is a function application, it could be enclosed within parens
      or not, according to the context. *)
  | Em_do_not_close
    (** Item is an expression that is intrinsically closed, parentheses are
      always spurious, whatever the context could be (such expressions are
      strings, chars, booleans, non negative numbers, ...). *)
;;

type context_kind =
  | Ck_toplevel
    (** Context dependent parens are useless (hence should be omitted). *)
  | Ck_record_field
    (** Context dependent parens are useless, except when the expression is a
      let expression. *)
  | Ck_tuple_item
    (** Only tuple items must be enclosed in parens. *)
  | Ck_argument
    (** All context dependent parens must be written. *)
;;

val must_parenthesize : enclosing_mode -> context_kind -> bool;;

val string_of_enclosing_mode : enclosing_mode -> string;;
val string_of_context_kind : context_kind -> string;;
