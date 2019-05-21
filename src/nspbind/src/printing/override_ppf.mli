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

(* $Id$ *)

type tag_new_line_markers = {
  m_tag_new_line : string;
  n_tag_new_line : string;
}
;;

val make_tag_printer : tag_new_line_markers ->
  'a Format_printer.printer -> 'a Format_printer.printer
;;
(* A printer transformer: turn a printer (a function with type
  [Format.formatter -> 'a -> unit]) into a printer with two semantic tags 
    defined m and n *) 

val matlab_printer : 'a Format_printer.printer -> 'a Format_printer.printer
;;
(* A printer transformer: turn a printer (a function with type
  [Format.formatter -> 'a -> unit]) into a matlab end of line aware printer.

  The resulting printer uses semantic tags to handle end of lines: tag [m]
  delimits a formatting area where end of lines are written the matlab way as
  [" ...\n"]; tag [n] delimits a formatting area where end of lines are
  treated the regular way as a simple ["\n"].
  Semantics tags [m] and [n] must be properly delimited with usual module
  [Format] semantics tag markers: open tag [m] (resp. tag [n]) using opening
  marker ["@{<m>"] (resp. marker ["@{<n>"]) and close tags using the closing
  marker ["@}"].
  Areas delimited with tags [m] and [n] could be nested.
  For simplicity, the empty tag is equivalent to tag [m], hence an area
  simply delimited between ["@{"] and ["@}"] will treat end of lines the
  matlab way.
*)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
