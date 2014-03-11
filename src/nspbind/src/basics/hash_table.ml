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

type ('a, 'b) t = ('a, 'b) Hashtbl.t;;

let create n =
  Hashtbl.create (Unatural_types.of_natural n)
;;

let bindings t =
  Hashtbl.fold (fun k v l -> (k, v) :: l) t []
;;

let values t =
  Hashtbl.fold (fun _k v l -> v :: l) t []
;;

let of_bindings bindings =
  let l = List.length bindings in
  let t = Hashtbl.create l in
  List.iter (fun (k, v) -> Hashtbl.add t k v) bindings;
  t
;;

(*
let hash_table_blit table eibt_bl =
  Hashtbl.iter (fun k v -> Hashtbl.replace eibt_bl k v) table
;;

let hash_table_append table eibt_bl =
  let res = Hashtbl.copy eibt_bl in
  hash_table_blit table res;
  res
;;
*)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
