(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2015,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type override_keywords =
  | OVERRIDE_ATTR
  | OVERRIDE_FIELD_VOID_POINTER_COPY
  | OVERRIDE_SLOT
  | HEADERS
  | COPYRIGHT
  | OVERRIDE_TYPE
  | OVERRIDE_IMPLEMENTS
  | OVERRIDE_SAVE_LOAD
  | OVERRIDE_SIZE
  | OVERRIDE_EQUAL
  | OVERRIDE_CREATE
  | OVERRIDE_INTCREATE
  | OVERRIDE_DESTROY_PRELIM
  | OVERRIDE_PRINT
  | OVERRIDE_INFO
  | OVERRIDE_PATH_EXTRACT
  | OVERRIDE_LOOP
  | OVERRIDE_INT_CREATE_FINAL
  | INCLUDE_PUBLIC
  | INCLUDE_START
  | INCLUDE_PRIVATE
  | OVERRIDE_INTERNAL_METHODS
  | OVERRIDE_INTERNAL_METHODS_PROTOS
  | INIT
  | LAST
  | MODULENAME
  | IMPORT
  | IGNORE
  | OVERRIDE
  | START
  | IGNORE_GLOB

val of_bindings : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val keyword_table : (string, override_keywords) Hashtbl.t
type code = string
type line = int
type value = code * line
type table = (string, value) Hashtbl.t
type overrides_keyword_table = (string, table) Hashtbl.t
val overrides_keyword_table : overrides_keyword_table
val get_name : string -> int -> int -> Buffer.t -> int
val get_names :
  string -> int -> int -> Buffer.t -> string list -> string list
val get_list_names : string -> string list
val store_in_table : string -> string -> value -> unit
type import_list = { mutable imports : string list; }
val import_list : import_list
val store_import : string -> string list -> unit
val get_imports : unit -> string list
val store : string -> string -> value -> unit
val store_ignore : 'a -> unit
val is : string -> string -> bool
val get : string -> string -> value
val get_copyright : unit -> code
val match_glob : string -> string -> bool
val is_ignore_glob_list : string list -> string -> bool
val is_ignored_glob : string -> bool
val is_ignored_std : string -> bool
val is_ignored : string -> bool

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
