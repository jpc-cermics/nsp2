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

(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)
type t = {
 loc_beg : position;
 loc_end : position;
}
;;

let mk_loc loc_beg loc_end = {
  loc_beg = loc_beg;
  loc_end = loc_end;
}
;;

let none =
  let pos_none =
    { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0; } in
  { loc_beg = pos_none; loc_end = pos_none; }
;;

let init lexbuf fname =
  lexbuf.Lexing.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let print ppf { loc_beg = loc_beg; loc_end = loc_end; } =
  Format.fprintf ppf
    "File \"%s\", %a"
    loc_beg.pos_fname
    Location.print_token_location (loc_beg, loc_end)
;;

let get_line { loc_beg = loc_beg; loc_end = _loc_end; } = 
  (* Printf.printf "Values  = %d %d %d \n" 
     loc_beg.pos_lnum loc_beg.pos_bol loc_beg.pos_cnum;
   *)
  loc_beg.pos_lnum ;

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
