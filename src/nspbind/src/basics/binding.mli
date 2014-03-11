(***********************************************************************)
(*                                                                     *)
(*                           Simport                                   *)
(*                                                                     *)
(*          Pierre Weis, INRIA Rocquencourt                            *)
(*                                                                     *)
(*  Copyright 2010-2013,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Ident;;

type 'a ident_binding = {
  bound_ident : ident;
  bound_value : 'a;
}

and 'a ident_bindings = ('a ident_binding) list

and 'a ident_binding_table = (ident, 'a ident_binding) Hash_table.t
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
