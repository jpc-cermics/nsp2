(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*                       EPI Serena, INRIA Paris                       *)
(*                                                                     *)
(*  Copyright (c) 1997-2018 INRIA.                                     *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Clement Franchini <Clement.Franchini@inria.fr>                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 Runtime library for pretty-printers generated with OCamlGen} *)

open Og_types;;

val escape_char : char -> string;;
val escape_string : string -> string;;

(** {6 Printing the Caml way} *)

(** {7 Basic printers} *)

val printer_Unit : unit printer;;
val printer_Bool : bool printer;;
val printer_Char : char printer;;
val printer_String : string printer;;
val printer_Bytes : Og_types.bytes printer;;

val printer_Int : int printer;;
val printer_Int32 : int32 printer;;
val printer_Int64 : int64 printer;;
val printer_Nativeint : nativeint printer;;
val printer_Float : float printer;;

(** {7 Printers for basic polymorphic values} *)

val printer_Format :
  ('a, 'b, 'c, ('a, 'b, 'c) format) printer_lifter3
;;
val printer_Format4 :
  ('a, 'b, 'c, 'd, ('a, 'b, 'c, 'd) format4) printer_lifter4
;;
val printer_Format6 :
  ('a, 'b, 'c, 'd, 'e, 'f, ('a, 'b, 'c, 'd, 'e, 'f) format6) printer_lifter6
;;

val printer_List : ('a, 'a list) printer_lifter;;
val printer_Array : ('a, 'a array) printer_lifter;;

val printer_Option : ('a, 'a option) printer_lifter;;
val printer_Ref : ('a, 'a ref) printer_lifter;;
val printer_Lazy : ('a, 'a Lazy.t) printer_lifter;;

(** {7 Printers for fully abstract values} *)

val printer_Fun : ('a, 'b, 'a -> 'b) printer_lifter2;;
val printer_Poly : 'a printer;;
val printer_Abstract : 'a printer;;
val printer_External : 'a printer;;

(** {6 Printing the user way} *)

(** {7 Basic user printers} *)

val printer_unit : unit printer;;
val printer_bool : bool printer;;
val printer_char : char printer;;
val printer_string : string printer;;
val printer_bytes : Og_types.bytes printer;;

val printer_int : int printer;;
val printer_int32 : int32 printer;;
val printer_int64 : int64 printer;;
val printer_nativeint : nativeint printer;;
val printer_float : float printer;;

(** {7 Printers for basic polymorphic values} *)

val printer_format :
  ('a, 'b, 'c, ('a, 'b, 'c) format) printer_lifter3
;;
val printer_format4 :
  ('a, 'b, 'c, 'd, ('a, 'b, 'c, 'd) format4) printer_lifter4
;;
val printer_format6 :
  ('a, 'b, 'c, 'd, 'e, 'f, ('a, 'b, 'c, 'd, 'e, 'f) format6) printer_lifter6
;;

val printer_list : ('a, 'a list) printer_lifter;;
val printer_array : ('a, 'a array) printer_lifter;;

val printer_option : ('a, 'a option) printer_lifter;;
val printer_ref : ('a, 'a ref) printer_lifter;;
val printer_lazy : ('a, 'a Lazy.t) printer_lifter;;

(** {7 User printers for fully abstract values} *)

val printer_fun : ('a, 'b, 'a -> 'b) printer_lifter2;;
val printer_poly : 'a printer;;
val printer_abstract : 'a printer;;
val printer_external : 'a printer;;
