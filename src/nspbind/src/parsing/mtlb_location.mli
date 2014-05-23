(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2014,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
;;

type t = {
  loc_beg : position;
  loc_end : position;
}
;;
(** The location of an AST node,
    beginning and ending position of its source text. *)

val init : Lexing.lexbuf -> Path.file_name -> unit;;
val mk_loc : position -> position -> t;;
val none : t;;
val print : Format.formatter -> t -> unit;;
val get_line : t -> int;;

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
