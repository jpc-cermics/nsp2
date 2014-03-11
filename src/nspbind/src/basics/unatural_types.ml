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

(* The private type of natural numbers. *)

let nt_failwith fun_name msg =
  failwith (Printf.sprintf "Unatural.%s: %s" fun_name msg)
;;

type natural = int;;

let to_natural i =
  if i < 0 then nt_failwith "of_int" "negative argument" else i
;;

external of_natural : natural -> int = "%identity";;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
