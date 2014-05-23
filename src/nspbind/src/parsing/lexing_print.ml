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

let print_position ppf = function
  | {
      Lexing.pos_fname = s;
      Lexing.pos_lnum = i;
      Lexing.pos_bol = i0;
      Lexing.pos_cnum = i1;
    } ->
    Format.fprintf ppf "%,@[<1>{@,";
    Format.fprintf ppf "%,@[<1>pos_fname =@ ";
    Lib_print.print_quoted_string ppf s; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>pos_lnum =@ ";
    Lib_print.print_quoted_int ppf i; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>pos_bol =@ ";
    Lib_print.print_quoted_int ppf i0; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>pos_cnum =@ ";
    Lib_print.print_quoted_int ppf i1; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@,}@]"
;;

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
