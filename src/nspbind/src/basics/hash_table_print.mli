(***********************************************************************)
(*                                                                     *)
(*                        SciCos compiler                              *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                               INRIA Rocquencourt                    *)
(*                                                                     *)
(*  Copyright 2010 INRIA                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val print_t :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) Hash_table.t -> unit
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
