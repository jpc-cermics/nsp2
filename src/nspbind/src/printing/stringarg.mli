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

val strip_type : string -> bool * string
type e_field = { e_tag : string; e_value : string; }

type enum = {
  is_enum : bool;
  e_name : string;
  e_c_name : string;
  e_values : e_field list;
  e_typecode : string;
  e_module : string;
}

type var_list = (string, string list) Hashtbl.t

type wrapper_info = {
  optional_args : bool;
  varargs : bool;
  varlist : string list * var_list;
  parsestr : string;
  opts : bool;
  parselist : string list;
  types : string list;
  codebefore : string list;
  attrcodebefore : string list;
  codeafter : string list;
  attrcodeafter : string list;
  attrcodecopy : string list;
  arglist : string list;
  kwlist : string list;
  tylist : string list;
  setobj : bool;
}

val wrapper_info : wrapper_info
val fresh_wrapper_info : unit -> wrapper_info
val get_tylist : wrapper_info -> string
val get_kwlist : wrapper_info -> string
val get_varlist : wrapper_info -> string
val get_parselist : wrapper_info -> string
val check_opts_in_parselist : string list -> bool
val get_arglist : wrapper_info -> string
val get_codebefore : wrapper_info -> string
val get_codeafter : wrapper_info -> string
val get_attrcodebefore : wrapper_info -> string
val get_attrcodeafter : wrapper_info -> string

type function_params = {
  ptype : string;
  pname : string;
  pdflt : string option;
  pnull : bool;
  psize : string;
  hidden : bool;
  pvarargs : bool;
}

type object_kind = Object | Interface | Struct | Pointer | Boxed

type object_rec = {
  or_name : string;
  or_module : string;
  or_parent : string;
  or_c_name : string;
  or_typecode : string;
  or_byref : bool;
  or_kind : object_kind;
  or_fields : function_params list;
  or_implements : string list;
  or_copy_func : string;
  or_release_func : string;
}

val check_gtk_class : object_rec -> bool

type function_obj = {
  f_name : string;
  f_c_name : string;
  f_varargs : bool;
  params : function_params list;
  ret : string option;
  caller_owns_return : bool option;
  deprecated : bool;
  deprecated_msg : string;
  is_method : bool;
  of_object : string;
  is_constructor_of : string;
  in_module : string;
  typecode : string;
  f_options : bool;
}
type ptype = string
type pname = string
type varname = string
type byref = bool
type ownsreturn = bool
type info = wrapper_info
type pdef = string
type psize = string
type pcheck = bool
type left_varname = string
type right_varname = string
type f_copy_name = string
type print_mode = string
type ftype = string
type fname = string
type opt = string

type string_arg = {
    write_param :
      string -> function_params -> wrapper_info -> bool -> wrapper_info;
  attr_write_set :
    string -> function_params -> wrapper_info -> bool -> wrapper_info;
  write_return : string -> bool -> wrapper_info -> wrapper_info;
  attr_write_return :
    object_rec -> ownsreturn -> function_params -> info -> wrapper_info;
  attr_free_fields : ptype -> pname -> varname -> byref -> string;
  attr_write_save : varname -> function_params -> byref -> string;
  attr_write_load : varname -> function_params -> byref -> string;
  attr_write_copy :
    object_rec ->
    function_params -> left_varname -> right_varname -> f_copy_name -> string;
  attr_write_info : ptype -> pname -> varname -> byref -> string;
  attr_write_print :
    object_rec -> print_mode -> varname -> function_params -> string;
  attr_write_init : object_rec -> varname -> function_params -> string;
  attr_equal_fields : object_rec -> varname -> function_params -> string;
  attr_write_defval : object_rec -> varname -> function_params -> string;
  attr_write_field_declaration : object_rec -> function_params -> string;
  attr_write_create_call : object_rec -> function_params -> bool -> string;
}

val is_in_list : 'a list -> 'a -> bool

val varlist_add :
  'a list * ('a, 'b list) Hashtbl.t ->
  'a -> 'b -> 'a list * ('a, 'b list) Hashtbl.t

val pset_name_set : bool -> string -> string -> string

val add_parselist :
  wrapper_info ->
  bool -> string -> string list -> string list -> wrapper_info

type parser = {
    mutable objects : object_rec list;
    mutable interfaces : object_rec list;
    mutable structures : object_rec list;
    mutable boxes : object_rec list;
    mutable pointers : object_rec list;
    mutable enums : enum list;
    mutable functions : function_obj list;
  }

val parser : parser
val register_parser : parser
val matcher_hash : (string, string_arg) Hashtbl.t
val matcher_get : string -> string_arg

val register_types : parser -> unit

(*
  Local Variables:
  compile-command: "cd ../..; make"
  End:
 *)
