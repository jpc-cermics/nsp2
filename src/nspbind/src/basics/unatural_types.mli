(***********************************************************************)
(*                                                                     *)
(*                                Ulib                                 *)
(*                                                                     *)
(*                   OCaml user's additional library                   *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright 2004-2013 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 The private type of natural numbers} *)

type natural = private int;;

val to_natural : int -> natural;;
external of_natural : natural -> int = "%identity";;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
