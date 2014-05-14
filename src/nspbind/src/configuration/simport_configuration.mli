(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type version = string;;

type source_file_types = 
  | Definition
  | Overrides 
;;

type configuration = {

  software_name : string;
  software_version : version;
        
  mutable verbose : bool;
  mutable debug : bool;
  mutable warnings : bool;

  mutable source_file_basename : Path.file_name option;
  mutable definitions_source_file : Path.file_name option;
  mutable overrides_source_file : Path.file_name option; 
  mutable target_file : Path.file_name option; 
  mutable prefix : string option;
    
  mutable path_to_override_for_c : Path.file_name option;
  mutable path_to_override_for_h : Path.file_name option;
}
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
