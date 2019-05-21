(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

type error =
  | Other of Override_location.t
;;

exception Error of error;;

let report_error ppf = function
  | Other _loc ->
      Format.fprintf ppf "Syntax error"
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
