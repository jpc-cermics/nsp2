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

(* Auxiliary type for reporting syntax errors *)

type error =
  | Other of Mtlb_location.t
;;

exception Error of error;;

val report_error: Format.formatter -> error -> unit;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
