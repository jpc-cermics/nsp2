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
 The data type definitions for precedences used by pretty-printers or
 readers provided by and generated with OCamlGen.

 OCamlGen generates pretty-printers or readers for values of user-defined
 types.

 OCamlGen provides pretty-printers or readers for values of any predefined
 OCaml type:
 * values of basic monomorphic type such as bool, string, char, or number
 types;
 * values of basic polymorphic type such as ref, option, list, array and
 tuple types.

 Type [enclosing_mode] specifies usage of delimiters around expression items.
 When a generated pretty-printer or reader should treat an item, it should
 print or read the item so as the item is either:
 - always enclosed with parens,
 - never enclosed with parens,
 - enclosed with parens in some context.

 Type [context_kind] is used to specify the context in which pretty-printers
 or readers are used (both provided and generated).

 The comparison function [use_delimiters] specifies the behavior of each
 [enclosing_mode] in each [context_kind] for OCaml syntax.
*)

type enclosing_mode =
  | Em_do_close
    (** Item must be enclosed within matching ``delimitors''
      (i.e. matching parens for expressions, matching brackets for list,
       matching bracket bars for array, or matching braces for record). *)
  | Em_tuple
    (** Item is a tuple, it must be enclosed or not within parens according
      to the context. *)
  | Em_application
    (** Item is a function application, it must be enclosed within parens
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

val use_delimiters : enclosing_mode -> context_kind -> bool;;

val string_of_enclosing_mode : enclosing_mode -> string;;
val string_of_context_kind : context_kind -> string;;
