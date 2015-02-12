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

val write_param : 'a -> function_params -> 'b -> 'c -> 'b
val write_return : 'a -> 'b -> 'c -> 'd
val attr_write_set : 'a -> 'b -> 'c -> 'd -> 'e
val attr_write_return : 'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val attr_write_copy : 'a -> 'b -> 'c -> 'd -> 'e -> string
val attr_write_save : 'a -> 'b -> 'c -> string
val attr_write_load : 'a -> 'b -> 'c -> string
val attr_write_info : 'a -> 'b -> 'c -> 'd -> string
val attr_write_print : 'a -> 'b -> 'c -> 'd -> string
val attr_write_init : 'a -> 'b -> 'c -> string
val attr_free_fields : 'a -> 'b -> 'c -> 'd -> string
val attr_equal_fields : 'a -> 'b -> 'c -> string
val attr_write_defval : 'a -> 'b -> 'c -> string
val attr_write_create_call : 'a -> function_params -> bool -> string
val attr_write_field_declaration : 'a -> function_params -> string
val argtype : string_arg

type enum_data = { enum_name : string; enum_typecode : string; }
val enum_arg_write_param :
    enum_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val enum_arg_attr_write_set :
    enum_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val enum_arg_write_return : 'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val enum_arg_attr_write_return :
    'a -> 'b -> 'c -> 'd -> wrapper_info -> wrapper_info
val enum_arg_attr_write_defval : 'a -> 'b -> 'c -> 'd -> string
val make_enum_arg : enum_data -> string_arg
val flag_arg_write_param :
    enum_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val flag_arg_attr_write_set :
    enum_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val flag_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val flag_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val flag_arg_attr_write_defval : 'a -> 'b -> 'c -> string
val make_flag_arg : enum_data -> string_arg

type nsp_generic_data = {
  ng_name : string;
  ng_fullname : string;
  ng_nsp_arg_type : string;
  ng_shortname : string;
  ng_shortname_uc : string;
}

type object_data = {
  od_objname : string;
  od_name : string;
  od_cast : string;
  od_parent : string;
  od_nsp_arg_type : string;
  od_shortname : string;
  od_shortname_uc : string;
}

val init_object_data : string -> string -> string -> string -> object_data
val nulldflt : string -> string -> string -> string
val dflt : string -> string -> string
val cast_name : object_data -> string

val object_arg_write_param :
    object_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val object_arg_attr_write_set :
    object_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val object_arg_write_return :
    'a -> string -> 'b -> wrapper_info -> wrapper_info
val object_arg_attr_write_return :
    'a -> 'b -> 'c -> function_params -> wrapper_info -> wrapper_info
val object_arg_attr_equal_fields :
    'a -> object_rec -> 'b -> function_params -> string
val object_arg_attr_free_fields :
    'a -> 'b -> string -> string -> bool -> string
val object_arg_attr_write_print :
    'a -> 'b -> string -> string -> function_params -> string
val object_arg_attr_write_init :
    'a -> 'b -> string -> function_params -> string
val object_arg_attr_write_defval :
    object_data -> 'a -> string -> function_params -> string
val object_arg_attr_write_copy :
    object_data ->
      'a -> function_params -> string -> string -> string -> string
val object_arg_attr_write_save :
    'a -> string -> function_params -> 'b -> string
val object_arg_attr_write_load :
    object_data -> string -> function_params -> 'a -> string
val make_object_arg : object_data -> string_arg
val gtk_object_arg_write_param :
    object_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val gtk_object_arg_write_return :
    object_data -> string -> bool -> wrapper_info -> wrapper_info
val gtk_object_arg_attr_write_return :
    object_data ->
      'a -> bool -> function_params -> wrapper_info -> wrapper_info
val make_gtk_object_arg : object_data -> string_arg
val nsp_object_arg_write_param :
    nsp_generic_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val nsp_object_arg_attr_write_set :
    nsp_generic_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val nsp_object_arg_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val nsp_object_arg_attr_write_return :
    'a -> 'b -> 'c -> 'd -> wrapper_info -> wrapper_info
val nsp_object_arg_attr_write_print :
    'a -> 'b -> 'c -> string -> function_params -> string
val nsp_object_arg_attr_write_init :
    nsp_generic_data -> 'a -> string -> function_params -> string
val nsp_object_arg_attr_equal_fields :
    'a -> object_rec -> 'b -> function_params -> string
val nsp_object_arg_attr_write_save :
    string -> function_params -> 'a -> string
val nsp_object_arg_attr_write_load :
    nsp_generic_data -> string -> function_params -> 'a -> string
val nsp_object_arg_attr_free_fields :
    'a -> 'b -> string -> string -> bool -> string
val nsp_object_arg_attr_write_copy :
    nsp_generic_data ->
      'a -> function_params -> string -> string -> string -> string
val nsp_object_arg_attr_write_defval :
    nsp_generic_data -> 'a -> string -> function_params -> string
val make_nsp_object_arg : nsp_generic_data -> string_arg
type struct_data = { sd_typename : string; sd_typecode : string; }
val struct_check : string -> string -> string -> string
val struct_null : string -> string -> string -> string
val struct_arg_write_param :
    struct_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val struct_arg_attr_write_set :
    struct_data ->
      string -> function_params -> wrapper_info -> bool -> wrapper_info
val struct_arg_write_return :
    struct_data -> string -> 'a -> wrapper_info -> wrapper_info
val struct_arg_attr_write_return :
    struct_data -> 'a -> 'b -> function_params -> wrapper_info -> wrapper_info
val struct_arg_attr_free_fields :
    'a -> string -> string -> 'b -> bool -> string
val struct_arg_attr_write_save : 'a -> function_params -> bool -> string
val struct_arg_attr_write_load :
    'a -> 'b -> function_params -> bool -> string
val rstrip : string -> string -> string
val struct_arg_attr_write_copy :
    'a -> 'b -> function_params -> string -> string -> string -> string
val struct_arg_attr_write_info : 'a -> string -> string -> 'b -> string
val struct_arg_attr_write_print :
    'a -> object_rec -> 'b -> string -> function_params -> string
val struct_arg_attr_write_init :
    'a -> 'b -> string -> function_params -> string
val struct_arg_attr_equal_fields :
    'a -> object_rec -> 'b -> function_params -> string
val struct_arg_attr_write_defval :
    'a -> object_rec -> 'b -> function_params -> string
val make_struct_arg : struct_data -> string_arg
type boxed_data = { bd_typename : string; bd_typecode : string; }
val boxed_check : string -> string -> string -> string
val boxed_null : string -> string -> string -> string

type custom_boxed_data = {
    cbd_getter : string;
    cbd_checker : string;
    cbd_new : string;
    cbd_nsp_type : string;
  }

val make_custom_boxed_arg : custom_boxed_data -> string_arg

type pointer_data = { pd_typename : string; pd_typecode : string; }

val pa_check : string -> string -> string -> string
val pa_null : string -> string -> string -> string
val pointer_arg_write_param :
    pointer_data -> 'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val pointer_arg_write_return :
    pointer_data -> string -> 'a -> wrapper_info -> wrapper_info
val pointer_arg_attr_write_return :
    pointer_data -> 'a -> 'b -> function_params -> wrapper_info -> wrapper_info
val make_pointer_arg : pointer_data -> string_arg
val atom : string -> string
val atom_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val atom_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val atom_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val atom_arg : string_arg
val gtype : string -> string
val gtype_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val gtype_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val gtype_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val gtype_arg : string_arg
val handle_gerror : string -> string
val gerror_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val gerror_arg : string_arg
val null1 : string -> string
val freepath : string -> string
val gtk_tree_path_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val gtk_tree_path_arg_write_return :
    'a -> bool -> wrapper_info -> wrapper_info
val gtk_tree_path_arg_attr_write_return :
    'a -> bool -> 'b -> wrapper_info -> wrapper_info
val gtk_tree_path_arg : string_arg
val normal : string -> string
val null : string -> string
val gdk_rectangle_pointer_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val gdk_rectangle_pointer_arg : string_arg
val gdk_rectangle_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val gdk_rectangle_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val gdk_rectangle_arg : string_arg
val nsp_glist_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val nsp_glist_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val nsp_glist_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val nsp_glist_arg : string_arg
val nsp_gslist_arg_write_param :
    'a -> function_params -> wrapper_info -> 'b -> wrapper_info
val nsp_gslist_arg_write_return : 'a -> 'b -> wrapper_info -> wrapper_info
val nsp_gslist_arg_attr_write_return :
    'a -> 'b -> 'c -> wrapper_info -> wrapper_info
val nsp_gslist_arg : string_arg
val print_vars : out_channel -> (string, string) Hashtbl.t -> unit
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
val register_object : object_rec -> unit
val register_enum : enum -> unit
val register_struct : object_rec -> unit
val register_get_name : object_rec -> string
val register_boxed : object_rec -> unit
val register_pointer : object_rec -> unit
val register_types : parser -> unit


(*
  Local Variables:
  compile-command: "cd ../..; make"
  End:
 *)
