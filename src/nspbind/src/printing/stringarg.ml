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

(* remove if found a trailing '*' *)

let strip_type s =
  let n = (String.length s) in
  let test = n > 0 && s.[n-1] = '*' in
  if test then
    (test, String.sub s 0 (n-1))
  else
    (test, s)
;;

(* enum datas *)

type e_field = {
    e_tag: string;
    e_value: string;
  }
;;

type enum = {
    is_enum: bool;
    e_name: string;
    e_c_name: string;
    e_values: e_field list;
    e_typecode: string;
    e_module:string;
    e_availability: string;
  }
;;

(* gather data for code generation *)

type var_list = ( string , string list ) Hashtbl.t;;

type wrapper_info = {
    optional_args: bool;
    varargs: bool;
    varlist: string list * var_list;
    parsestr: string;
    opts: bool;
    parselist: string list;
    types:string list;
    codebefore: string list;
    attrcodebefore: string list;
    codeafter: string list;
    attrcodeafter: string list;
    attrcodecopy: string list;
    arglist: string list;
    kwlist: string list;
    tylist: string list;
    setobj: bool;
  }
;;

let wrapper_info =
  {
   optional_args = false;
   varargs = false;
   varlist= ([], Hashtbl.create 256);
   parsestr= "wrapper_info_string";
   opts= false;
   parselist= [];
   types=[];
   codebefore= [];
   attrcodebefore= [];
   codeafter= [];
   attrcodeafter= [];
   attrcodecopy= [];
   arglist= [];
   kwlist= [];
   tylist= [];
   setobj= false;
 }
;;

let fresh_wrapper_info () = { wrapper_info with varlist= ([], Hashtbl.create 256);}
;;

let get_tylist info =
  let tylist = List.rev info.tylist in
  let tylist =
    if info.optional_args then tylist @ ["new_opts"] else
    tylist in
  if List.length tylist = 0 then
    ""
  else
    let str  = List.fold_right
	(fun x arg ->  if arg = "" then x else x ^ "," ^ arg)
	tylist "" in
    Printf.sprintf "  int_types T[] = {%s, t_end};\n" str
;;

let get_kwlist info =
  if List.length info.kwlist <> 0 then
    "  nsp_option opts[] = {" ^
    (List.fold_right (fun x arg -> x ^ "," ^ arg ) (List.rev info.kwlist) "")
    ^ "\n\t{NULL,t_end,NULLOBJ,-1} };\n"
  else
    ""
;;

let get_varlist info =
  let (l, tbl ) = info.varlist in
  List.fold_right
    (fun x rest ->
      let values = Hashtbl.find tbl x in
      let value =
	List.fold_right
	  (fun x arg -> if arg = "" then x else x ^ ", " ^ arg ) (List.rev values) "" in
      (Format.sprintf "  %s %s;\n" x value) ^ rest )
    (List.rev l) ""
;;

let get_parselist info =
  let rec concat list =
    match list with
    | [] -> ""
    | [val1] -> val1
    | val1 :: rest ->
	Printf.sprintf "%s, %s" val1 (concat rest) in
  concat (List.rev info.parselist)
;;

let rec check_opts_in_parselist parselist =
  match parselist with
  | [] -> false
  | "opts" :: _parselist -> true
  | _ :: parselist -> check_opts_in_parselist parselist
;;

let get_arglist info =
  List.fold_right
    (fun x arg -> if arg = "" then x else x ^ "," ^ arg )
    (List.rev info.arglist) ""
;;

let get_codebefore info =
  List.fold_right (fun x arg -> x ^ arg ) (List.rev  info.codebefore) ""
;;

let get_codeafter info =
  List.fold_right (fun x arg -> x ^ arg ) (List.rev info.codeafter) ""
;;

let get_attrcodebefore info =
  List.fold_right (fun x arg -> x ^ arg ) (List.rev info.attrcodebefore) ""
;;

let get_attrcodeafter info =
  List.fold_right (fun x arg -> x ^ arg ) info.attrcodeafter ""
;;

(* function or method arguments or object field  *)

type function_params ={
    ptype: string;
    pname: string;
    pdflt: string option;
    pnull: bool;
    psize: string;
    hidden: bool;
    pvarargs: bool;
  }
;;

(* class or interface as obtained from lisp file *)

type object_kind =
  | Object | Interface | Struct | Pointer | Boxed
;;

type object_rec = {
    or_name: string;
    or_module: string;
    or_parent: string;
    or_c_name: string;
    or_typecode: string;
    or_byref: bool;
    or_kind: object_kind;
    or_fields: function_params list;
    or_implements: string list;
    or_copy_func: string;
    or_release_func: string;
    or_availability: string;
  }
;;

let check_gtk_class objinfo =
  objinfo.or_parent = "GObject" ||
  objinfo.or_module = "Atk" ||
  objinfo.or_module = "Pango"  ||
  objinfo.or_module = "Gdk" ||
  objinfo.or_module = "Gtk" ||
  objinfo.or_module = "WebKit" ||
  objinfo.or_module = "Gio" ||
  objinfo.or_module = "G_TYPE" ||
  objinfo.or_module = "Cairo" ||
  objinfo.or_module = "GI" 
;;

(* a function or a method *)

type function_obj = {
    f_name: string;(* name of function or method *)
    f_c_name: string; (* name of c function to call *)
    f_varargs: bool;(* some arguments are optionals *)
    params: function_params list; (* parameters of function *)
    ret: string option;  (* type of returned value *)
    caller_owns_return: bool option; (* ? *)
    deprecated: bool;(* true if function is deprecated *)
    deprecated_msg: string;(* true if function is deprecated *)
    is_method: bool;(* flag to decide between method and function *)
    of_object: string;(* used for methods *)
    is_constructor_of: string;
    in_module: string;  (* XXX *)
    typecode: string;
    f_options: bool; (* some arguments are named options *)
    availability: string;
    calling_code: string;
  }
;;

(* should be unused *)

type ptype = string ;;
type pname = string ;;
type varname = string;;
type byref = bool;;
type ownsreturn = bool;;
type info = wrapper_info;;
type pdef = string;;
type psize = string;;
type pcheck = bool;;
type left_varname = string;;
type right_varname = string;;
type f_copy_name = string;;
type print_mode = string;;
type ftype = string;;
type fname = string;;
type opt = string;;

type string_arg =
  {
   write_param:  string -> function_params -> wrapper_info -> bool -> wrapper_info;
   attr_write_set:  string -> function_params -> wrapper_info -> bool -> wrapper_info;
   write_return:  string -> bool -> wrapper_info -> wrapper_info;
   attr_write_return: object_rec -> ownsreturn -> function_params -> info -> wrapper_info;
   attr_free_fields: ptype -> pname -> varname -> byref -> string;
   attr_write_save: varname -> function_params -> byref -> string;
   attr_write_load:  varname -> function_params -> byref -> string;
   attr_write_copy:  object_rec -> function_params ->left_varname -> right_varname -> f_copy_name -> string;
   attr_write_info: ptype -> pname -> varname -> byref -> string;
   attr_write_print:  object_rec -> print_mode -> varname -> function_params ->string;
   attr_write_init: object_rec -> varname -> function_params ->string;
   attr_equal_fields: object_rec -> varname -> function_params -> string;
   attr_write_defval: object_rec -> varname -> function_params ->string;
   attr_write_field_declaration: object_rec -> function_params ->string;
   attr_write_create_call: object_rec -> function_params -> bool -> string;
   }
;;

let rec is_in_list list value =
  match list with
  | [] -> false
  | x :: rest ->
      if x = value then true
      else  is_in_list rest value
;;

let varlist_add varlist ctype name =
  let (l, tbl ) = varlist in
  if is_in_list l ctype then
    (
     Hashtbl.replace tbl ctype ( name :: (Hashtbl.find tbl ctype));
     (l, tbl )
    )
  else
    (
     Hashtbl.add tbl ctype [name];
     (ctype :: l , tbl)
    )
;;

let pset_name_set byref c_name fname =
  if byref then (Printf.sprintf "((%s *) self)->obj->%s" c_name fname )
  else (Printf.sprintf "((%s *) self)->%s" c_name fname)
;;

let add_parselist info opts codes parseargs keywords =
  let kwlist_fold acc x =
    (Printf.sprintf "\n\t{\"%s\",%s,NULLOBJ,-1}" x codes ) :: acc in
  let kwlist =
    if opts then
      List.fold_left kwlist_fold info.kwlist keywords
    else
      info.kwlist in
  let tylist_fold acc _x =
    (Printf.sprintf "%s" codes ) :: acc in
  let tylist =
    if opts then
      info.tylist
    else
      List.fold_left tylist_fold info.tylist keywords in

  let parseargs = if opts then
    (
     if check_opts_in_parselist info.parselist then
       parseargs
     else
       "opts" :: parseargs
    )
  else
    parseargs in
  let parselist = (List.rev parseargs) @ info.parselist in
  { info with
    parsestr =  info.parsestr ^ codes;
    parselist =  parselist;
    opts = opts;
    tylist = tylist;
    kwlist = kwlist;
  }

(* dealing with arguments of functions/methods *)

(* defaults functions *)

let write_param _oname params info _byref=
  Printf.printf "write_param not implemented for %s\n%!" params.ptype;
  info
;;

let write_return _ptype _ownsreturn _info =
  failwith (Printf.sprintf "write_return not implemented for %s" "self.class.name")
;;

let attr_write_set _oname _params _info _byref=
  failwith (Printf.sprintf  "attr_write_set not implemented for %s" "self.class.name")
;;

let attr_write_return _objinfo _ownsreturn _params info=
  let code = Printf.sprintf "  XXXXX attr_write_return not implemented for %s\n" "self.class.name" in
  { info with attrcodeafter = code :: info.attrcodeafter ;}
;;

let attr_write_copy _objinfo _params _left_varname _right_varname _f_copy_name =
  Printf.sprintf "  XXXXX attr_write_copy not implemented for %s" "self.class.name"
;;

let attr_write_save _varname _params _byref=
  Printf.sprintf "  XXXXX attr_write_save not implemented for %s\n" "self.class.name"
;;

let attr_write_load _varname _params _byref=
  Printf.sprintf "  XXXXX attr_write_load not implemented for %s\n" "self.class.name"
;;

let attr_write_info _ptype _pname _varname _byref =
  Printf.sprintf "  XXXXX attr_write_info not implemented for %s\n" "self.class.name"
;;

let attr_write_print _objinfo _print_mode _varname _params =
  Printf.sprintf "  XXXXX attr_write_print not implemented for %s\n" "self.class.name"
;;

let attr_write_init _objinfo _varname _params =
  Printf.sprintf "  XXXXX attr_write_init not implemented for %s\n" "self.class.name"
;;

let attr_free_fields _ptype _pname _varname __byref =
  Printf.sprintf "  XXXXX attr_free_fields not implemented for %s\n" "_self.class.name"
;;

let attr_equal_fields _objinfo _varname _params=
  Printf.sprintf "  XXXXX attr_equal_fields not implemented for %s\n" "_self.class.name"
;;

let attr_write_defval _objinfo _varname _params =
  Printf.sprintf "  XXXXX attr_write_defval not implemented for %s\n" "_self.class.name"
;;

let attr_write_create_call _objinfo params flag =
  let fftype = if flag then "" else params.ptype in
  let fftype = if fftype = "double[]" then "double*" else fftype in
  Printf.sprintf "%s %s" fftype params.pname
;;

let attr_write_field_declaration _objinfo params=
  if params.ptype = "double[]" then
    Printf.sprintf "  double %s[%s];\n"  params.pname params.psize
  else
    Printf.sprintf "  %s %s;\n" params.ptype params.pname

let argtype =
  {
   write_param = write_param;
   attr_write_set = attr_write_set;
   write_return = write_return;
   attr_write_return = attr_write_return ;
   attr_free_fields = attr_free_fields ;
   attr_write_save = attr_write_save ;
   attr_write_load = attr_write_load ;
   attr_write_copy = attr_write_copy ;
   attr_write_info = attr_write_info ;
   attr_write_print = attr_write_print ;
   attr_write_init = attr_write_init ;
   attr_equal_fields = attr_equal_fields ;
   attr_write_defval = attr_write_defval ;
   attr_write_field_declaration =    attr_write_field_declaration;
   attr_write_create_call = attr_write_create_call;
  }
;;

(* stringarg: argument of type "string" *)

let stringarg_write_param oname params info byref=
  let pname = params.pname in
  let init_value =
    match params.pdflt with
    | None -> Printf.sprintf "*%s" pname
    | Some "NULL" -> Printf.sprintf "*%s = NULL"  pname
    | Some x -> Printf.sprintf "*%s = \"%s\"" pname x in
  let varlist = varlist_add info.varlist "char"  init_value in
  let info = { info with arglist = pname :: info.arglist; varlist = varlist;} in
  let info = add_parselist info params.pvarargs "string" ["&" ^ pname] [pname] in
  let attrcodebefore =
    (Printf.sprintf "  if ((%s = nsp_string_object(O))==NULL) return FAIL;\n"  pname)
    ^ (Printf.sprintf  "  if ((%s = nsp_string_copy(%s)) ==NULL) return FAIL;\n" pname pname)
    ^ (
      if  byref then
	(Printf.sprintf "  nsp_string_destroy(&((%s *) self)->obj->%s);\n" oname pname)
      else
	(Printf.sprintf "  nsp_string_destroy(&((%s *) self)->%s);\n"  oname pname)) in
  { info with  attrcodebefore = attrcodebefore :: info.attrcodebefore;}
;;

let stringarg_attr_write_set oname params info byref =
  let pset_name = pset_name_set byref oname params.pname in
  let info = stringarg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let stringarg_write_return _ptype ownsreturn info =
  let varlist =
    if ownsreturn then
      varlist_add info.varlist "gchar" "*ret"
    else
      varlist_add info.varlist "const gchar" "*ret"
  in
  let codeafter =
    if ownsreturn then
      "  if ( nsp_move_string(stack,1,(ret) ? ret: \"\",-1)== FAIL) return RET_BUG;\n" ^
      "  g_free(ret);\n  return 1;"
    else
      "  if ( nsp_move_string(stack,1,(ret) ? ret: \"\",-1)== FAIL) return RET_BUG;\n" ^
      "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let stringarg_attr_write_return _objinfo ownsreturn _params info=
  let varlist = varlist_add info.varlist "NspObject" "*nsp_ret" in
  let varlist, attrcodeafter =
    if ownsreturn then
      ( varlist_add varlist "gchar" "*ret" ,
	"  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  g_free(ret);\n  return nsp_ret;")
    else
      ( varlist_add varlist "const gchar" "*ret" ,
	"  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  return nsp_ret;" ) in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let stringarg_attr_free_fields _ptype pname _varname _byref =
  (Printf.sprintf "  nsp_string_destroy(&(%s->%s));\n"  _varname pname )
;;

let stringarg_attr_write_save _varname params _byref=
  Printf.sprintf "  if (nsp_xdr_save_string(xdrs,%s->%s) == FAIL) return FAIL;\n"
    _varname params.pname
;;

let stringarg_attr_write_load varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_new_string(xdrs,&(%s->%s)) == FAIL) return NULL;\n"
     varname params.pname)
;;

let stringarg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (Printf.sprintf "  if ((%s->%s = nsp_string_copy(%s->%s)) == NULL) return NULL;\n"
       left_varname params.pname right_varname params.pname)
  else
    (Printf.sprintf "  %s->%s = %s;\n" left_varname params.pname params.pname )
;;

let stringarg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%s\\n\",%s->%s);\n"  pname _varname pname)
;;

let stringarg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s|=\\\\verb@\\\"%%s\\\"@\\n\",(%s->%s==NULL) ? \"NULL\": %s->%s);\n"
      params.pname varname params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%s\\n\",%s->%s);\n"
      params.pname varname params.pname
;;

let stringarg_attr_write_init _objinfo varname params=
  match params.pdflt with
  | None
  | Some "NULL" ->
      (Printf.sprintf "  %s->%s = NULL;\n"  varname params.pname )
  | Some x ->
      (Printf.sprintf "  %s->%s = nsp_new_string(\"%s\",-1);\n"
	 varname params.pname x)
;;

let stringarg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  (Printf.sprintf "  if ( strcmp(A->%s,loc->%s) != 0) return FALSE;\n"  pname pname)
;;

let stringarg_attr_write_defval _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL) \n    {\n"  varname params.pname ) ^
  (Printf.sprintf "  if (( %s->%s = nsp_string_copy(\"\")) == NULL)\n       return FAIL;\n    }\n"
     varname params.pname )
;;

let stringarg =
  { argtype with
    write_param = stringarg_write_param;
    attr_write_set = stringarg_attr_write_set;
    write_return = stringarg_write_return;
    attr_write_return = stringarg_attr_write_return ;
    attr_free_fields = stringarg_attr_free_fields ;
    attr_write_save = stringarg_attr_write_save ;
    attr_write_load = stringarg_attr_write_load ;
    attr_write_copy = stringarg_attr_write_copy ;
    attr_write_info = stringarg_attr_write_info ;
    attr_write_print = stringarg_attr_write_print ;
    attr_write_init = stringarg_attr_write_init ;
    attr_equal_fields = stringarg_attr_equal_fields ;
    attr_write_defval = stringarg_attr_write_defval ;
  }
;;

(* nonearg: argument of type none *)

let nonearg_write_return _ptype _ownsreturn info =
  { info with codeafter = "  return 0;"  :: info.codeafter ;}
;;

let nonearg_attr_write_return _objinfo _ownsreturn _params info=
  { info with attrcodeafter = "  return NULLOBJ;"  :: info.attrcodeafter ;}
;;

let nonearg_attr_write_defval _objinfo _varname _params =
  Printf.sprintf ""
;;

let nonearg =
  { argtype with
    write_return = nonearg_write_return;
    attr_write_return = nonearg_attr_write_return ;
    attr_write_defval = nonearg_attr_write_defval ;
  }
;;

(* uchararg: argument of type unsigned char *)

let uchararg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "guchar" ("*" ^ params.pname )
    | Some x ->
	varlist_add info.varlist "guchar"
	  (Printf.sprintf "*%s = \"%s\"" params.pname x) in
  let info = { info with arglist = params.pname :: info.arglist; varlist = varlist;} in
  if params.pnull then
    add_parselist info params.pvarargs "string"  ["&" ^ params.pname]   [params.pname]
  else
    add_parselist info params.pvarargs "string"  ["&" ^ params.pname]   [params.pname]
;;

let uchararg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = uchararg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let uchararg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let uchararg =
  { argtype with
    write_param = uchararg_write_param;
    attr_write_set = uchararg_attr_write_set;
    attr_write_defval = uchararg_attr_write_defval ;
  }
;;

(* chararg: a character argument is an int at nsp level *)

let chararg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "int" params.pname
    | Some x ->
	varlist_add info.varlist "int"  (params.pname ^ " = \"" ^ x ^ "\"") in
  let info = { info with arglist = params.pname :: info.arglist; varlist = varlist;} in
  add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname]
;;

let chararg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = chararg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let chararg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "int"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^
    "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let chararg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "int"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let chararg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let chararg =
  { argtype with
    write_param = chararg_write_param;
    attr_write_set = chararg_attr_write_set;
    write_return = chararg_write_return;
    attr_write_return = chararg_attr_write_return ;
    attr_write_defval = chararg_attr_write_defval ;
  };;

(* gunichararg:  a 1 character unicode string *)

let gunichararg_write_param _oname params info _byref=
  let param_tmpl pname = Printf.sprintf  "  %s = (gunichar)nsp_%s;\n" pname pname in
  let dflt_tmpl pname =
    Printf.sprintf "  if (nsp_%s != NULL) {\n" pname
    ^ Printf.sprintf "      if (nsp_%s[1] != 0) {\n" pname
    ^ Printf.sprintf "          Scierror( \"Error: %s should be a 1 character unicode string\\n\");\n" pname
    ^ Printf.sprintf "          return RET_BUG;\n"
    ^ Printf.sprintf "      }\n"
    ^ Printf.sprintf "      %s = (gunichar)nsp_%s[0];\n" pname pname
    ^ Printf.sprintf "   }\n" in
  let (varlist, codebefore) =
    match params.pdflt with
    | None ->
	(varlist_add info.varlist "gunichar"  params.pname, param_tmpl params.pname)
    | Some x ->
	(varlist_add info.varlist "gunichar"  (params.pname ^ " = \"" ^ x ^ "\""),
	 dflt_tmpl params.pname) in
  let varlist = varlist_add varlist "int"  ("nsp_" ^ params.pname ^ " = 0") in
  let info = { info with
	       arglist = params.pname :: info.arglist;
	       varlist = varlist;
	       codebefore = codebefore :: info.codebefore;} in
  add_parselist info params.pvarargs "s_int"  ["&nsp_" ^ params.pname]  [params.pname]
;;

let gunichararg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = gunichararg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let gunichararg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "gunichar"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;\n  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}

;;
let gunichararg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "gunichar"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let gunichararg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let gunichararg =
  { argtype with
    write_param = gunichararg_write_param;
    attr_write_set = gunichararg_attr_write_set;
    write_return = gunichararg_write_return;
    attr_write_return = gunichararg_attr_write_return ;
    attr_free_fields = attr_free_fields ;
    attr_write_save = attr_write_save ;
    attr_write_load = attr_write_load ;
    attr_write_copy = attr_write_copy ;
    attr_write_info = attr_write_info ;
    attr_write_print =  attr_write_print ;
    attr_write_init = attr_write_init ;
    attr_equal_fields = attr_equal_fields ;
    attr_write_defval = gunichararg_attr_write_defval ;
  }
;;

(* intarg: int argument which is at nsp level a real (double) 1x1 matrix *)

let intarg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "int"  params.pname
    | Some x ->
	varlist_add info.varlist "int"  (params.pname ^ " = " ^ x ) in
  let info = { info with arglist = params.pname :: info.arglist; varlist = varlist;} in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with attrcodebefore= (Printf.sprintf "  if ( IntScalar(O,&%s) == FAIL) return FAIL;\n" params.pname )
    :: info.attrcodebefore ;}
;;

let intarg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = intarg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let intarg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "int"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^ "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let intarg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "int"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let intarg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ right_varname  ^"->" ^ params.pname  ^";\n")
  else
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ params.pname  ^ ";\n")
;;

let intarg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s) == FAIL) return FAIL;\n"
     _varname params.pname )
;;

let intarg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_i(xdrs, &%s->%s) == FAIL) return NULL;\n"  _varname params.pname )
;;

let intarg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%d\\n\" %s->%s);\n"  pname _varname pname)
;;

let intarg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s|= \\\\numprint{%%d}\\n\",%s->%s);\n"
      params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%d\\n\", %s->%s);\n"
    params.pname varname params.pname
;;

let intarg_attr_write_init _objinfo varname params =
    match params.pdflt with
    | None ->  Printf.sprintf "  %s->%s = 0;\n"  varname params.pname
    | Some x -> Printf.sprintf "  %s->%s = %s;\n"  varname params.pname x
;;

let intarg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let intarg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->" ^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let intarg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let intarg =
  { argtype with
    write_param = intarg_write_param;
    attr_write_set = intarg_attr_write_set;
    write_return = intarg_write_return;
    attr_write_return = intarg_attr_write_return ;
    attr_free_fields = intarg_attr_free_fields ;
    attr_write_save = intarg_attr_write_save ;
    attr_write_load = intarg_attr_write_load ;
    attr_write_copy = intarg_attr_write_copy ;
    attr_write_info = intarg_attr_write_info ;
    attr_write_print = intarg_attr_write_print ;
    attr_write_init = intarg_attr_write_init ;
    attr_equal_fields = intarg_attr_equal_fields ;
    attr_write_defval = intarg_attr_write_defval ;
  }
;;

(* # when used as a parameter in an interface; an "int*" is  *)
(* # transmited through pointer at nsp level it is an integer  *)
(* # i.e 1x1 real mat used by reference  *)
(* # when used as an object field name foo a foo_length field  *)
(* # is added to record the length of foo  *)

(* int64_pointer_arg: *)

let int64_pointer_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (Printf.sprintf
       "  if ((%s->%s = malloc(%s->%s_length*sizeof(gint64)))== NULL) return NULL;\n"
       left_varname params.pname right_varname params.pname)  ^
    (Printf.sprintf
       "  %s->%s_length = %s->%s_length;\n"
       left_varname params.pname right_varname params.pname)  ^
    (Printf.sprintf
       "  memcpy(%s->%s,%s->%s,%s->%s_length*sizeof(gint64));\n"
       left_varname params.pname right_varname params.pname right_varname params.pname)
  else
    (Printf.sprintf "  %s->%s = %s;\n" left_varname params.pname params.pname ) ^
    (Printf.sprintf "  %s->%s_length = %s_length;\n" left_varname params.pname params.pname )
;;

let int64_pointer_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None ->
      Printf.sprintf "  %s->%s = NULL; %s->%s_length = 0; \n"
	varname params.pname varname params.pname
  | Some x ->
      Printf.sprintf "  %s->%s = %s;\n"  varname params.pname x
;;

let int64_pointer_arg_attr_write_print _objinfo _print_mode _varname _params =
  (* # XXX to be done  *)
  (Printf.sprintf "" )
;;

let int64_pointer_arg_attr_free_fields _ptype pname _varname _byref =
  (Printf.sprintf "    FREE(%s->%s);\n"  _varname pname )
;;

let int64_pointer_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s_length) == FAIL) return FAIL;\n"  _varname params.pname ) ^
  (Printf.sprintf "  if (nsp_xdr_save_array_i(xdrs, %s->%s, %s->%s_length) == FAIL) return FAIL;\n"
     _varname params.pname _varname params.pname)
;;

let int64_pointer_arg_attr_write_load _varname params _byref=
  (Printf.sprintf  "  if (nsp_xdr_load_i(xdrs,&(%s->%s_length)) == FAIL) return NULL;\n"
     _varname params.pname ) ^
  (Printf.sprintf "  if ((%s->%s = malloc(%s->%s_length*sizeof(gint64)))== NULL) return NULL;\n"
     _varname params.pname _varname params.pname) ^
  (Printf.sprintf"  if (nsp_xdr_load_array_i(xdrs,%s->%s,%s->%s_length) == FAIL) return NULL;\n"
     _varname params.pname _varname params.pname)
;;

let int64_pointer_arg_attr_write_create_call _objinfo params flag =
  let fftype = if flag then "" else params.ptype in
  (Printf.sprintf "%s %s, int %s_length" fftype params.pname params.pname)
;;

let int64_pointer_arg_attr_write_set oname params info _byref=
  let pset_name = pset_name_set _byref oname params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname ^ " = " ^ x) in
  let varlist = varlist_add varlist "int"  "i" in
  let varlist = varlist_add varlist "int"  ("*pi=" ^ pset_name  ) in
  let varlist = varlist_add varlist "int"  "*loc = NULL"  in
  let info = add_parselist info params.pvarargs "mat_int"  ["&" ^ params.pname]  [params.pname] in
  let codebefore =
    (Printf.sprintf "  if ( ! IsMat(O)  ||  ((NspMatrix *) O)->rc_type != 'r' ) return FAIL; \n") ^
    (Printf.sprintf "  %s = (NspMatrix *) O; \n" params.pname) ^
    (Printf.sprintf "  if ((loc = malloc( %s_length*sizeof(gint64)))== NULL) return FAIL;\n" pset_name) ^
    (Printf.sprintf "  FREE(pi); pi = loc;\n") ^
    (Printf.sprintf "  %s_length = %s->mn;\n" pset_name params.pname) ^
    (Printf.sprintf "  for ( i = 0 ; i < %s->mn ; i++) pi[i]= (gint64) %s->R[i];\n"  params.pname params.pname)  in
  { info with
    arglist = (params.pname ^ "->I") :: info.arglist;
    varlist = varlist;
    attrcodebefore = codebefore :: info.attrcodebefore;
  }
;;

let int64_pointer_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->" ^ params.pname else params.pname in
  "  {int i;\n"
  ^ (Printf.sprintf "    for ( i = 0 ; i < A->%s_length ; i++)\n" pname)
  ^ (Printf.sprintf"      if ( A->%s[i] != loc->%s[i]) return FALSE;\n"  pname pname)
  ^ "  }\n"
;;

let int64_pointer_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let int64_pointer_arg_attr_write_field_declaration _objinfo params=
  (Printf.sprintf  "  %s %s;  int %s_length;\n" params.ptype params.pname params.pname)
;;

let int64_pointer_arg_attr_write_return objinfo _ownsreturn params info=
  (* # used for returning an attribute value  *)
  (* # this is done by copying the associated field  *)
  (* # some fields have been set for  *)
  let pset_name = pset_name_set objinfo.or_byref objinfo.or_c_name params.pname in
  let varlist = varlist_add info.varlist "ZZZgint64"  "*ret" in
  let varlist = varlist_add varlist "NspMatrix"  "*nsp_ret" in
  let code = (Printf.sprintf "  if (( nsp_ret = nsp_matrix_create(NVOID,'r',1,%s_length)) == NULL) return NULL;\n" pset_name)
    ^ (Printf.sprintf "  memcpy(nsp_ret->I, ret , %s_length*sizeof(int));\n"  pset_name)
    ^ "  nsp_ret->convert = \"i\";\n"
    ^ "  return NSP_OBJECT(nsp_ret);" in
  { info with
    attrcodeafter = code :: info.attrcodeafter;
    varlist = varlist;
  }
;;

let int64_pointer_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "gint64"  "*ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) *ret)==FAIL) return RET_BUG;\n" ^  "  return 1;"  in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let int64_pointer_arg =
  {
    argtype with
    (* argtype with write_param = int64_pointer_arg_write_param; *)
    attr_write_set = int64_pointer_arg_attr_write_set;
    write_return = int64_pointer_arg_write_return;
    attr_write_return = int64_pointer_arg_attr_write_return ;
    attr_free_fields = int64_pointer_arg_attr_free_fields ;
    attr_write_save = int64_pointer_arg_attr_write_save ;
    attr_write_load = int64_pointer_arg_attr_write_load ;
    attr_write_copy = int64_pointer_arg_attr_write_copy ;
    (* attr_write_info = int64_pointer_arg_attr_write_info ; *)
    attr_write_print = int64_pointer_arg_attr_write_print ;
    attr_write_init = int64_pointer_arg_attr_write_init ;
    attr_equal_fields = int64_pointer_arg_attr_equal_fields ;
    attr_write_defval = int64_pointer_arg_attr_write_defval ;
    attr_write_create_call = int64_pointer_arg_attr_write_create_call;
    attr_write_field_declaration =  int64_pointer_arg_attr_write_field_declaration;
  }
;;

(* int_pointer_arg: *)

let int_pointer_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "int"  params.pname
    | Some x ->
	varlist_add info.varlist "int"  (params.pname ^ " = " ^ x ) in
  let info = { info with arglist = ("&" ^ params.pname) :: info.arglist; varlist = varlist;} in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with attrcodebefore= (Printf.sprintf "  if ( IntScalar(O,&%s) == FAIL) return FAIL;\n" params.pname )
    :: info.attrcodebefore ;}
;;

let int_pointer_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (* # this part is used in copy or full_copy  *)
    (Printf.sprintf  "  if ((%s->%s = malloc(%s->%s_length*sizeof(int)))== NULL) return NULL;\n"
       left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  %s->%s_length = %s->%s_length;\n"  left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  memcpy(%s->%s,%s->%s,%s->%s_length*sizeof(int));\n"
         left_varname params.pname right_varname params.pname right_varname params.pname)
  else
    (* # this part is only used on create and we do not want to copy the given string  *)
    (* # note that if the given string is NULL it will be set to "" by check_values.  *)
    (* # (Printf.sprintf "  if ((%s->%s = nsp_string_copy(%s)) == NULL) return NULL;\n"
       left_varname params.pname params.pname ) *)
    (Printf.sprintf"  %s->%s = %s;\n" left_varname params.pname params.pname )
    ^  (Printf.sprintf"  %s->%s_length = %s_length;\n" left_varname params.pname params.pname )
;;

let int_pointer_arg_attr_write_init _objinfo varname params =
    match params.pdflt with
    | None -> Printf.sprintf "  %s->%s = NULL; %s->%s_length = 0; \n" varname params.pname varname params.pname
    | Some x ->
	Printf.sprintf "  %s->%s = %s;\n" varname params.pname x
;;

let int_pointer_arg_attr_write_print _objinfo _print_mode _varname _params =
  (Printf.sprintf "" )
;;

let int_pointer_arg_attr_free_fields _ptype pname _varname _byref =
  (Printf.sprintf "    FREE(%s->%s);\n"  _varname pname )
;;

let int_pointer_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s_length) == FAIL) return FAIL;\n"  _varname params.pname )
  ^ (Printf.sprintf "  if (nsp_xdr_save_array_i(xdrs, %s->%s, %s->%s_length) == FAIL) return FAIL;\n"
       _varname params.pname _varname params.pname)
;;

let int_pointer_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_i(xdrs,&(%s->%s_length)) == FAIL) return NULL;\n"   _varname params.pname )
   ^ (Printf.sprintf "  if ((%s->%s = malloc(%s->%s_length*sizeof(int)))== NULL) return NULL;\n"
        _varname params.pname _varname params.pname)
  ^ (Printf.sprintf "  if (nsp_xdr_load_array_i(xdrs,%s->%s,%s->%s_length) == FAIL) return NULL;\n"
       _varname params.pname _varname params.pname)
;;

let int_pointer_arg_attr_write_create_call _objinfo params flag=
  let fftype = if flag then "" else params.ptype in
  (* # for int pointers we add the length in a generated extra field  *)
  (Printf.sprintf "%s %s, int %s_length" fftype params.pname params.pname)
;;

let int_pointer_arg_attr_write_set oname params info _byref=
  let pset_name = pset_name_set _byref oname params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname ^ " = " ^ x) in
  let varlist = varlist_add varlist "int"  "i"  in
  let varlist = varlist_add varlist "int"  ( "*pi=" ^ pset_name  ) in
  let varlist = varlist_add varlist "int"  "*loc = NULL" in

  let codebefore = (Printf.sprintf "  if ( ! IsMat(O)  ||  ((NspMatrix *) O)->rc_type != 'r' ) return FAIL; \n")
  ^ (Printf.sprintf "  %s = (NspMatrix *) O; \n"  params.pname)
  ^ (Printf.sprintf "  if ((loc = malloc( %s_length*sizeof(int)))== NULL) return FAIL;\n"  pset_name)
  ^ (Printf.sprintf "  FREE(pi); pi = loc;\n" )
  ^ (Printf.sprintf "  %s_length = %s->mn;\n" pset_name params.pname)
  ^ (Printf.sprintf "  for ( i = 0 ; i < %s->mn ; i++) pi[i]= (int) %s->R[i];\n"  params.pname params.pname)  in

  let info = add_parselist info params.pvarargs "mat_int"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = (params.pname ^ "->I") :: info.arglist;
    varlist = varlist;
    attrcodebefore = codebefore :: info.attrcodebefore;
  }
;;

let int_pointer_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  "  {int i;\n"
  ^ (Printf.sprintf "    for ( i = 0 ; i < A->%s_length ; i++)\n" pname)
  ^ (Printf.sprintf "      if ( A->%s[i] != loc->%s[i]) return FALSE;\n"  pname pname)
  ^ "  }\n"
;;

let int_pointer_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let int_pointer_arg_attr_write_field_declaration _objinfo params=
  (Printf.sprintf "  %s %s;  int %s_length;\n" params.ptype params.pname params.pname)
;;

let int_pointer_arg_attr_write_return objinfo _ownsreturn params info=
  (* # used for returning an attribute value  *)
  (* # this is done by copying the associated field  *)
  (* # some fields have been set for  *)
  let pset_name = pset_name_set objinfo.or_byref objinfo.or_c_name params.pname in
  let varlist = varlist_add info.varlist "NspMatrix"  "*nsp_ret" in
  let varlist = varlist_add varlist "int"  "*ret" in
  let str = (Printf.sprintf "  if (( nsp_ret = nsp_matrix_create(NVOID,'r',1,%s_length)) == NULL) return NULL;\n"
	       pset_name)
    ^ (Printf.sprintf "  memcpy(nsp_ret->I, ret , %s_length*sizeof(int));\n"  pset_name)
    ^ "  nsp_ret->convert = \"i\";\n"
    ^ "  return NSP_OBJECT(nsp_ret);" in
  { info with varlist = varlist ; attrcodeafter = str :: info.attrcodeafter ;}
;;

let int_pointer_arg_write_return _ptype  _ownsreturn info =
  let varlist = varlist_add  info.varlist "int"  "*ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) *ret)==FAIL) return RET_BUG;\n" ^  "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let int_pointer_arg =
  { argtype with
    write_param = int_pointer_arg_write_param;
    attr_write_set = int_pointer_arg_attr_write_set;
    write_return = int_pointer_arg_write_return;
    attr_write_return = int_pointer_arg_attr_write_return ;
    attr_free_fields = int_pointer_arg_attr_free_fields ;
    attr_write_save = int_pointer_arg_attr_write_save ;
    attr_write_load = int_pointer_arg_attr_write_load ;
    attr_write_copy = int_pointer_arg_attr_write_copy ;
    (* attr_write_info = int_pointer_arg_attr_write_info ;  *)
    attr_write_print = int_pointer_arg_attr_write_print ;
    attr_write_init = int_pointer_arg_attr_write_init ;
    attr_equal_fields = int_pointer_arg_attr_equal_fields ;
    attr_write_defval = int_pointer_arg_attr_write_defval ;
    attr_write_create_call = int_pointer_arg_attr_write_create_call;
    attr_write_field_declaration =  int_pointer_arg_attr_write_field_declaration;
 }
;;

(* double_pointer_arg *)

let double_pointer_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "double"  params.pname
    | Some x ->
	varlist_add info.varlist "double"  (params.pname ^ " = " ^ x ) in
  let info = { info with arglist = ("&" ^ params.pname) :: info.arglist; varlist = varlist;} in
  let info = add_parselist info params.pvarargs "s_double"  ["&" ^ params.pname]  [params.pname] in
  { info with attrcodebefore= (Printf.sprintf "  if ( DoubleScalar(O,&%s) == FAIL) return FAIL;\n" params.pname )
    :: info.attrcodebefore ;}
;;

let double_pointer_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "double"  "*ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) *ret)==FAIL) return RET_BUG;\n" ^ "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let double_pointer_arg_attr_write_return objinfo _ownsreturn params info=
  (* # used for returning an attribute value  *)
  (* # this is done by copying the associated field  *)
  (* # some fields have been set for  *)
  let pset_name = pset_name_set objinfo.or_byref objinfo.or_c_name params.pname in
  let varlist = varlist_add info.varlist "NspMatrix"  "*nsp_ret" in
  let varlist = varlist_add varlist "double"  "*ret" in
  let str =
    (Printf.sprintf"  if (( nsp_ret = nsp_matrix_create(NVOID,'r',1,%s_length)) == NULL) return NULL;\n"
       pset_name)
    ^ (Printf.sprintf"  memcpy(nsp_ret->R,ret , %s_length*sizeof(int));\n"  pset_name)
    ^ "  return NSP_OBJECT(nsp_ret);" in
  { info with varlist = varlist ; attrcodeafter = str :: info.attrcodeafter ;}
;;

let double_pointer_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (* # this part is used in copy or full_copy  *)
    (Printf.sprintf "  if ((%s->%s = malloc(%s->%s_length*sizeof(double)))== NULL) return NULL;\n"
       left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  %s->%s_length = %s->%s_length;\n" left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  memcpy(%s->%s,%s->%s,%s->%s_length*sizeof(double));\n"
	 left_varname params.pname right_varname params.pname right_varname params.pname)
  else
    (* # this part is only used on create and we do not want to copy the given string  *)
    (* # note that if the given string is NULL it will be set to "" by check_values.  *)
    (* # (Printf.sprintf "  if ((%s->%s = nsp_string_copy(%s)) == NULL) return NULL;\n" left_varname params.pname params.pname ) *)
    (Printf.sprintf "  %s->%s = %s;\n" left_varname params.pname params.pname )
    ^ (Printf.sprintf"  %s->%s_length = %s_length;\n" left_varname params.pname params.pname )
;;

let double_pointer_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None -> Printf.sprintf "  %s->%s = NULL; %s->%s_length = 0; \n" varname params.pname varname params.pname
  | Some x ->
      Printf.sprintf "  %s->%s = AFAIRE %s;\n"  varname params.pname x
;;

let double_pointer_arg_attr_write_print _objinfo _print_mode _varname _params =
  (* # XXX to be done  *)
  (Printf.sprintf "" )
;;

let double_pointer_arg_attr_free_fields _ptype pname _varname _byref =
  (Printf.sprintf "    FREE(%s->%s);\n"  _varname pname )
;;

let double_pointer_arg_attr_write_save _varname params _byref=
  (Printf.sprintf"  if (nsp_xdr_save_i(xdrs, %s->%s_length) == FAIL) return FAIL;\n"  _varname params.pname )
  ^ (Printf.sprintf"  if (nsp_xdr_save_array_d(xdrs, %s->%s, %s->%s_length) == FAIL) return FAIL;\n"
       _varname params.pname _varname params.pname)
;;

let double_pointer_arg_attr_write_load _varname params _byref=
  (Printf.sprintf"  if (nsp_xdr_load_i(xdrs,&(%s->%s_length)) == FAIL) return NULL;\n"   _varname params.pname )
  ^ (Printf.sprintf"  if ((%s->%s = malloc(%s->%s_length*sizeof(double)))== NULL) return NULL;\n"
       _varname params.pname _varname params.pname)
  ^ (Printf.sprintf"  if (nsp_xdr_load_array_d(xdrs,%s->%s,%s->%s_length) == FAIL) return NULL;\n"
       _varname params.pname _varname params.pname)
;;

let double_pointer_arg_attr_write_create_call _objinfo params flag=
  let fftype = if flag then "" else params.ptype in
  (* # for int pointers we add the length in a generated extra field  *)
  (Printf.sprintf "%s %s, int %s_length" fftype params.pname params.pname)

let double_pointer_arg_attr_write_set oname params info _byref=
  let pset_name = pset_name_set _byref oname params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname ^ " = " ^ x) in
  let varlist =varlist_add varlist "int"  "i" in
  let varlist =varlist_add varlist "double"  ("*pi=" ^ pset_name  )in
  let varlist =varlist_add varlist "double"  "*loc = NULL" in
  let codebefore = "  if ( ! IsMat(O)  ||  ((NspMatrix *) O)->rc_type != 'r' ) return FAIL; \n"
    ^ (Printf.sprintf "  %s = (NspMatrix *) O; \n" params.pname)
    ^ (Printf.sprintf "  if ((loc = malloc( %s_length*sizeof(double)))== NULL) return FAIL;\n"  pset_name)
    ^ (Printf.sprintf "  FREE(pi); pi = loc;\n" )
    ^ (Printf.sprintf "  %s_length = %s->mn;\n" pset_name params.pname)
    ^ (Printf.sprintf "  for ( i = 0 ; i < %s->mn ; i++) pi[i]= %s->R[i];\n"  params.pname params.pname)  in
  let info = add_parselist info params.pvarargs "mat_int"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = (params.pname ^ "->R") :: info.arglist;
    varlist = varlist;
    attrcodebefore = codebefore :: info.attrcodebefore;
  }
;;

let double_pointer_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  "  {int i;\n"
  ^ (Printf.sprintf"    for ( i = 0 ; i < A->%s_length ; i++)\n" pname)
  ^ (Printf.sprintf"      if ( A->%s[i] != loc->%s[i]) return FALSE;\n"  pname pname)
  ^ "  }\n"
;;

let double_pointer_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let double_pointer_arg_attr_write_field_declaration _objinfo params=
  (Printf.sprintf "  %s %s;  int %s_length;\n" params.ptype params.pname params.pname)
;;

let double_pointer_arg =
  { argtype with
    write_param = double_pointer_arg_write_param;
    attr_write_set = double_pointer_arg_attr_write_set;
    write_return = double_pointer_arg_write_return;
    attr_write_return = double_pointer_arg_attr_write_return ;
    attr_free_fields = double_pointer_arg_attr_free_fields ;
    attr_write_save = double_pointer_arg_attr_write_save ;
    attr_write_load = double_pointer_arg_attr_write_load ;
    attr_write_copy = double_pointer_arg_attr_write_copy ;
    (* attr_write_info = double_pointer_arg_attr_write_info ;  *)
    attr_write_print = double_pointer_arg_attr_write_print ;
    attr_write_init = double_pointer_arg_attr_write_init ;
    attr_equal_fields = double_pointer_arg_attr_equal_fields ;
    attr_write_defval = double_pointer_arg_attr_write_defval ;
    attr_write_create_call = double_pointer_arg_attr_write_create_call;
    attr_write_field_declaration =  double_pointer_arg_attr_write_field_declaration;
 }
;;

(* bool_pointer_arg *)

let bool_pointer_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "int"  params.pname
    | Some x ->
	varlist_add info.varlist "int"  (params.pname ^ " = " ^ x ) in
  let info = { info with arglist = ("&" ^ params.pname) :: info.arglist; varlist = varlist;} in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with attrcodebefore= (Printf.sprintf "  if ( BoolScalar(O,&%s) == FAIL) return FAIL;\n" params.pname )
    :: info.attrcodebefore ;}
;;

let bool_pointer_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (* # this part is used in copy or full_copy  *)
    (Printf.sprintf "  if ((%s->%s = malloc(%s->%s_length*sizeof(Boolean)))== NULL) return NULL;\n"
       left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  %s->%s_length = %s->%s_length;\n"
	 left_varname params.pname right_varname params.pname)
    ^ (Printf.sprintf"  memcpy(%s->%s,%s->%s,%s->%s_length*sizeof(Boolean));\n"
	 left_varname params.pname right_varname params.pname right_varname params.pname)
  else
    (* # this part is only used on create and we do not want to copy the given string  *)
    (* # note that if the given string is NULL it will be set to "" by check_values.  *)
    (* # (Printf.sprintf "  if ((%s->%s = nsp_string_copy(%s)) == NULL) return NULL;\n"
       left_varname params.pname params.pname ) *)
    (Printf.sprintf "  %s->%s = %s;\n" left_varname params.pname params.pname )
    ^ (Printf.sprintf"  %s->%s_length = %s_length;\n" left_varname params.pname params.pname )
;;

let bool_pointer_arg_attr_write_init _objinfo varname params =
    match params.pdflt with
    | None ->
	Printf.sprintf "  %s->%s = NULL; %s->%s_length = 0; \n"
	  varname params.pname varname params.pname
    | Some x -> Printf.sprintf "  %s->%s = AFAIRE %s;\n"  varname params.pname x
;;

let bool_pointer_arg_attr_write_print _objinfo _print_mode _varname _params =
  (Printf.sprintf "" )
;;

let bool_pointer_arg_attr_free_fields _ptype pname _varname _byref =
  (Printf.sprintf "    FREE(%s->%s);\n"  _varname pname )
;;

let bool_pointer_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s_length) == FAIL) return FAIL;\n"  _varname params.pname )
  ^ (Printf.sprintf
       "  if (nsp_xdr_save_array_i(xdrs, %s->%s, %s->%s_length) == FAIL) return FAIL;\n"
       _varname params.pname _varname params.pname)
;;

let bool_pointer_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_i(xdrs,&(%s->%s_length)) == FAIL) return NULL;\n"
     _varname params.pname )
  ^ (Printf.sprintf "  if ((%s->%s = malloc(%s->%s_length*sizeof(Boolean)))== NULL) return NULL;\n"
       _varname params.pname _varname params.pname)
  ^ (Printf.sprintf "  if (nsp_xdr_load_array_i(xdrs,%s->%s,%s->%s_length) == FAIL) return NULL;\n"
       _varname params.pname _varname params.pname)
;;

let bool_pointer_arg_attr_write_create_call _objinfo params flag=
  let fftype = if flag then "" else params.ptype in
  (* # for int pointers we add the length in a generated extra field  *)
  (Printf.sprintf "%s %s, int %s_length" fftype params.pname params.pname)
;;

let bool_pointer_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspBMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "NspBMatrix"  ("*" ^ params.pname ^ " = " ^ x) in
  let varlist = varlist_add varlist "int"  "i" in
  let varlist = varlist_add varlist "int"  ("*pi=" ^ pset_name  ) in
  let varlist = varlist_add varlist "int"  "*loc = NULL" in
  let codebefore = (Printf.sprintf "  if ( ! IsBMat(O)) return FAIL; \n")
    ^ (Printf.sprintf "  %s = (NspBMatrix *) O; \n" params.pname)
    ^ (Printf.sprintf "  if ((loc = malloc( %s_length*sizeof(int)))== NULL) return FAIL;\n"  pset_name)
    ^ (Printf.sprintf "  FREE(pi); pi = loc;\n" )
    ^ (Printf.sprintf "  %s_length = %s->mn;\n" pset_name params.pname)
    ^ (Printf.sprintf "  for ( i = 0 ; i < %s->mn ; i++) pi[i]= %s->B[i];\n"  params.pname params.pname) in
  let info = add_parselist info params.pvarargs "mat_int"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = (params.pname ^ "->B") :: info.arglist;
    varlist = varlist;
    attrcodebefore = codebefore :: info.attrcodebefore;
  }
;;

let bool_pointer_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  "  {int i;\n"
  ^ (Printf.sprintf "    for ( i = 0 ; i < A->%s_length ; i++)\n" pname)
  ^ (Printf.sprintf "      if ( A->%s[i] != loc->%s[i]) return FALSE;\n"  pname pname)
  ^ "  }\n"
;;

let bool_pointer_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let bool_pointer_arg_attr_write_field_declaration _objinfo params=
  (Printf.sprintf "  %s %s;  int %s_length;\n" params.ptype params.pname params.pname)
;;

let bool_pointer_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "Boolean"  "*ret" in
  let codeafter = "  if ( nsp_move_boolean(stack,1, *ret)==FAIL) return RET_BUG;\n" ^  "  return 1;"  in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let bool_pointer_arg_attr_write_return objinfo _ownsreturn params info=
  (* # used for returning an attribute value  *)
  (* # this is done by copying the associated field  *)
  (* # some fields have been set for  *)
  let pset_name = pset_name_set objinfo.or_byref objinfo.or_c_name params.pname in
  let varlist = varlist_add info.varlist "int"  "*ret" in
  let varlist = varlist_add varlist "NspBMatrix"  "*nsp_ret" in
  let str = (Printf.sprintf "  if (( nsp_ret = nsp_bmatrix_create(NVOID,1,%s_length)) == NULL) return NULL;\n"
	       pset_name)
    ^ (Printf.sprintf "  memcpy(nsp_ret->B, ret , %s_length*sizeof(int));\n"  pset_name)
    ^ (Printf.sprintf "  return NSP_OBJECT(nsp_ret);" ) in
  { info with varlist = varlist ; attrcodeafter = str :: info.attrcodeafter ;}
;;

let bool_pointer_arg =
  { argtype with
    write_param = bool_pointer_arg_write_param;
    attr_write_set = bool_pointer_arg_attr_write_set;
    write_return = bool_pointer_arg_write_return;
    attr_write_return = bool_pointer_arg_attr_write_return ;
    attr_free_fields = bool_pointer_arg_attr_free_fields ;
    attr_write_save = bool_pointer_arg_attr_write_save ;
    attr_write_load = bool_pointer_arg_attr_write_load ;
    attr_write_copy = bool_pointer_arg_attr_write_copy ;
    (* attr_write_info = bool_pointer_arg_attr_write_info ;  *)
    attr_write_print = bool_pointer_arg_attr_write_print ;
    attr_write_init = bool_pointer_arg_attr_write_init ;
    attr_equal_fields = bool_pointer_arg_attr_equal_fields ;
    attr_write_defval = bool_pointer_arg_attr_write_defval ;
    attr_write_create_call = bool_pointer_arg_attr_write_create_call;
    attr_write_field_declaration =  bool_pointer_arg_attr_write_field_declaration;
 }
;;

(* argument of type boolean *)

let boolean_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "int"  params.pname
    | Some x -> varlist_add info.varlist "int"  (params.pname ^ " = " ^ x) in
  let attrcodebefore = (Printf.sprintf "  if ( BoolScalar(O,&" ^ params.pname ^ ") == FAIL) return FAIL;\n") in
  let info = add_parselist info params.pvarargs "s_bool"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    attrcodebefore = attrcodebefore :: info.attrcodebefore;
  }
;;

let boolean_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = boolean_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let boolean_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "int"  "ret" in
  let codeafter = "  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;\n" ^  "  return 1;"  in
  let attrcodeafter = "  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n  return nsp_ret;" in
  { info with
    varlist = varlist ;
    codeafter = codeafter :: info.codeafter ;
    attrcodeafter = attrcodeafter :: info.attrcodeafter ;
  }
;;

let boolean_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "int"  "ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let code = "  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n" ^
    "return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = code :: info.attrcodeafter ;}
;;

let boolean_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s) == FAIL) return FAIL;\n"
     _varname params.pname )
;;

let boolean_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_i(xdrs, &%s->%s) == FAIL) return NULL;\n"  _varname params.pname )
;;


let boolean_arg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s\t= %%s\\n\"  ( %s->%s == TRUE) ? \"T\" : \"F\" );\n"
     pname _varname pname)
;;
let boolean_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ right_varname  ^"->" ^ params.pname  ^";\n")
  else
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ params.pname  ^ ";\n")
;;

let boolean_arg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s|= %%s\\n\",( %s->%s == TRUE) ? \"T\" : \"F\" );\n"
      params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s\t= %%s\\n\", ( %s->%s == TRUE) ? \"T\" : \"F\" );\n"
      params.pname varname params.pname
;;

let boolean_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None -> Printf.sprintf "  %s->%s = TRUE;\n" varname params.pname
  | Some x -> Printf.sprintf "  %s->%s = %s;\n" varname params.pname x
;;

let boolean_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let boolean_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->" ^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let boolean_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let boolean_arg =
  { argtype with
    write_param = boolean_arg_write_param;
    attr_write_set = boolean_arg_attr_write_set;
    write_return = boolean_arg_write_return;
    attr_write_return = boolean_arg_attr_write_return ;
    attr_free_fields = boolean_arg_attr_free_fields ;
    attr_write_save = boolean_arg_attr_write_save ;
    attr_write_load = boolean_arg_attr_write_load ;
    attr_write_copy = boolean_arg_attr_write_copy ;
    attr_write_info = boolean_arg_attr_write_info ;
    attr_write_print = boolean_arg_attr_write_print ;
    attr_write_init = boolean_arg_attr_write_init ;
    attr_equal_fields = boolean_arg_attr_equal_fields ;
    attr_write_defval = boolean_arg_attr_write_defval ;
  }
;;

(* time_t_write_param *)

let time_t_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->       varlist_add info.varlist "time_t"  params.pname
    | Some x -> varlist_add info.varlist "time_t"  (params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
  }
;;

let time_t_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = time_t_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let time_t_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "time_t"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^  "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let time_t_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "time_t"  "ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let str = "  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = str :: info.attrcodeafter ;}
;;

let time_t_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let time_t_arg =
  { argtype with
    write_param = time_t_write_param;
    attr_write_set = time_t_attr_write_set;
    write_return = time_t_write_return;
    attr_write_return = time_t_attr_write_return ;
    (* attr_free_fields = time_t_attr_free_fields ;*)
    (*attr_write_save = time_t_attr_write_save ;*)
    (*attr_write_load = time_t_attr_write_load ;*)
    (*attr_write_copy = time_t_attr_write_copy ;*)
    (*attr_write_info = time_t_attr_write_info ;*)
    (*attr_write_print = time_t_attr_write_print ;*)
    (*attr_write_init = time_t_attr_write_init ;*)
    (*attr_equal_fields = time_t_attr_equal_fields ;*)
    attr_write_defval = time_t_attr_write_defval ;
  }
;;

(* ulong_arg *)

let ulong_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->  varlist_add info.varlist "gulong"  params.pname
    | Some x -> varlist_add info.varlist "gulong"  (params.pname ^ " = " ^ x) in
  let attrcodebefore = (Printf.sprintf "  if ( ULongScalar(O,&" ^ params.pname ^ ") == FAIL) return FAIL;\n") in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    attrcodebefore = attrcodebefore :: info.attrcodebefore;
  }
;;

let ulong_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = ulong_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let ulong_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "gulong"  "ret" in
  let codeafter = " if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;\n" ^  "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let ulong_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "gulong"  "ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let attrcodeafter = "  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let ulong_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let ulong_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let ulong_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_i(xdrs, %s->%s) == FAIL) return FAIL;\n"
     _varname params.pname )
;;
let ulong_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_i(xdrs, &%s->%s) == FAIL) return NULL;\n"  _varname params.pname )
;;

let ulong_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ right_varname  ^"->" ^ params.pname  ^";\n")
  else
    ("  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ params.pname  ^ ";\n")
;;

let ulong_arg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%d\\n\" %s->%s);\n"  pname _varname pname)
;;

let ulong_arg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s|= \\\\numprint{%%d}\\n\",%s->%s);\n"
      params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%d\\n\", %s->%s);\n"
      params.pname varname params.pname
;;

let ulong_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None -> Printf.sprintf "  %s->%s = 0;\n" varname params.pname
  | Some x -> Printf.sprintf "  %s->%s = %s;\n" varname params.pname x
;;

let ulong_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->" ^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let ulong_arg =
  { argtype with
    write_param = ulong_arg_write_param;
    attr_write_set = ulong_arg_attr_write_set;
    write_return = ulong_arg_write_return;
    attr_write_return = ulong_arg_attr_write_return ;
    attr_free_fields = ulong_arg_attr_free_fields ;
    attr_write_save = ulong_arg_attr_write_save ;
    attr_write_load = ulong_arg_attr_write_load ;
    attr_write_copy = ulong_arg_attr_write_copy ;
    attr_write_info = ulong_arg_attr_write_info ;
    attr_write_print = ulong_arg_attr_write_print ;
    attr_write_init = ulong_arg_attr_write_init ;
    attr_equal_fields = ulong_arg_attr_equal_fields ;
    attr_write_defval = ulong_arg_attr_write_defval ;
  }
;;

(* int64 *)

let int64_arg_write_param _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None -> varlist_add info.varlist "gint64"  params.pname
    | Some x -> varlist_add info.varlist "gint64" (params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs "s_int"  ["&" ^ params.pname]  [params.pname] in
  { info with arglist = params.pname :: info.arglist; varlist = varlist;}
;;

let int64_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = int64_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let int64_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "gint64"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^  "  return 1;"  in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let int64_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "gint64"  "ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let attrcodeafter = "  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let int64_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let int64_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None -> Printf.sprintf "  %s->%s = 0;\n" varname params.pname
  | Some x ->
      Printf.sprintf "  %s->%s = %s;\n" varname params.pname x
;;

let int64_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let int64_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let int64_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ right_varname  ^"->" ^ params.pname  ^";\n"
  else
    "  " ^ left_varname ^ "->" ^ params.pname  ^"=" ^ params.pname  ^";\n"
;;

let int64_arg_attr_write_print _objinfo _print_mode varname params =
  Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%ld\\n\", %s->%s);\n"
    params.pname varname params.pname
;;

let int64_arg =
  { argtype with
    write_param = int64_arg_write_param;
    attr_write_set = int64_arg_attr_write_set;
    write_return = int64_arg_write_return;
    attr_write_return = int64_arg_attr_write_return ;
    attr_free_fields = int64_arg_attr_free_fields ;
    (* attr_write_save = int64_arg_attr_write_save ; *)
    (* attr_write_load = int64_arg_attr_write_load ; *)
    attr_write_copy = int64_arg_attr_write_copy ;
    (* attr_write_info = int64_arg_attr_write_info ; *)
    attr_write_print = int64_arg_attr_write_print ;
    attr_write_init = int64_arg_attr_write_init ;
    attr_equal_fields = int64_arg_attr_equal_fields ;
    attr_write_defval = int64_arg_attr_write_defval ;
  }
;;

(* uint64_arg *)

let dflt pname =
  Printf.sprintf "  if (nsp_%s)\n" pname ^
  Printf.sprintf "      %s = nsp_int_to_uint64(nsp_%s);\n" pname pname
;;

let before pname =
   Printf.sprintf "  %s = nsp_int_to_uint64(nsp_%s);\n" pname pname ;;

let uint64_arg_write_param _oname params info _byref=
  let varlist, code =
    match params.pdflt with
    | None ->
	( varlist_add info.varlist "guint64"  params.pname,
	  before params.pname)
    | Some x ->
	( varlist_add info.varlist "guint64"  (params.pname ^ " = " ^ x),
	  dflt params.pname) in
  let varlist = varlist_add varlist "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
  let info = add_parselist info params.pvarargs "obj_check"  ["&nsp_type_imatrix"; ("&nsp_" ^ params.pname)]  [params.pname] in
  { info with
    arglist = params.pname :: info.arglist;
    codebefore = code :: info.codebefore;
    varlist = varlist;
  }

let uint64_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = uint64_arg_write_param oname params info byref in
  { info with
    attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let uint64_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "guint64"  "ret" in
  let codeafter = "  return nsp_int_from_uint64(ret);" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let uint64_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "guint64"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let uint64_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let uint64_arg =
  { argtype with
    write_param = uint64_arg_write_param;
    attr_write_set = uint64_arg_attr_write_set;
    write_return = uint64_arg_write_return;
    attr_write_return = uint64_arg_attr_write_return ;
    (* attr_free_fields = uint64_arg_attr_free_fields ; *)
    (* attr_write_save = uint64_arg_attr_write_save ; *)
    (* attr_write_load = uint64_arg_attr_write_load ; *)
    (* attr_write_copy = uint64_arg_attr_write_copy ; *)
    (* attr_write_info = uint64_arg_attr_write_info ; *)
    (* attr_write_print = uint64_arg_attr_write_print ; *)
    (* attr_write_init = uint64_arg_attr_write_init ; *)
    (* attr_equal_fields = uint64_arg_attr_equal_fields ; *)
    attr_write_defval = uint64_arg_attr_write_defval ;
  };;

(* argument of type double *)

let double_arg_write_param _oname params info _byref=
  let value =
    match params.pdflt with
    | None -> params.pname
    | Some x -> params.pname ^ " = " ^ x in
  let varlist = varlist_add info.varlist "double" value in
  let info = add_parselist info params.pvarargs "s_double"  ["&" ^ params.pname]  [params.pname] in
  let attrcodebefore = (Printf.sprintf "  if ( DoubleScalar(O,&" ^ params.pname ^ ") == FAIL) return FAIL;\n") in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    attrcodebefore = attrcodebefore :: info.attrcodebefore;
  }
;;

let double_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = double_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let double_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "double"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;\n" ^
    "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let double_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "double"  "ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let attrcodeafter = "  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let double_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let double_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  " ^  left_varname ^ "->" ^  params.pname  ^ "=" ^  right_varname  ^ "->" ^  params.pname  ^ ";\n"
  else
    "  " ^  left_varname ^ "->" ^  params.pname  ^ "=" ^  params.pname  ^ ";\n"
;;

let double_arg_attr_write_save _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_save_d(xdrs, %s->%s) == FAIL) return FAIL;\n"  _varname params.pname )
;;

let double_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  if (nsp_xdr_load_d(xdrs, &%s->%s) == FAIL) return NULL;\n"  _varname params.pname )
;;

let double_arg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%f\\n\" %s->%s);\n"  pname _varname pname)
;;

let double_arg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s| = \\\\numprint{%%f}\\n\", %s->%s);\n"
      params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s=%%f\\n\", %s->%s);\n"
      params.pname varname params.pname
;;

let double_arg_attr_write_init _objinfo varname params =
  match params.pdflt with
  | None -> Printf.sprintf "  %s->%s = 0.0;\n"  varname params.pname
  | Some x -> Printf.sprintf "  %s->%s = %s;\n"  varname params.pname x
;;

let double_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let double_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let double_arg =
  { argtype with
    write_param = double_arg_write_param;
    attr_write_set = double_arg_attr_write_set;
    write_return = double_arg_write_return;
    attr_write_return = double_arg_attr_write_return ;
    attr_free_fields = double_arg_attr_free_fields ;
    attr_write_save = double_arg_attr_write_save ;
    attr_write_load = double_arg_attr_write_load ;
    attr_write_copy = double_arg_attr_write_copy ;
    attr_write_info = double_arg_attr_write_info ;
    attr_write_print = double_arg_attr_write_print ;
    attr_write_init = double_arg_attr_write_init ;
    attr_equal_fields = double_arg_attr_equal_fields ;
    attr_write_defval = double_arg_attr_write_defval ;
  }
;;

(* file_arg *)

let nulldflt pname =
  Printf.sprintf "  if ( IsNOne(nsp_%s)\n"  pname ^
  Printf.sprintf "      %s = NULL;\n"  pname ^
  Printf.sprintf "  else if (nsp_%s && IsFile(nsp_%s)\n"  pname pname ^
  Printf.sprintf "      %s = PyFile_AsFile(nsp_%s);\n"  pname  pname ^
  Printf.sprintf "  else if (nsp_%s) {\n"  pname  ^
  Printf.sprintf "      Scierror( \"Error: %s should be a file object or None\\n\");\n"  pname  ^
  Printf.sprintf "      return RET_BUG;\n"  ^
  Printf.sprintf "  }"
;;

let null pname =
  Printf.sprintf "  if (nsp_%s && PyFile_Check(nsp_%s)\n"  pname  pname ^
  Printf.sprintf "      %s = PyFile_AsFile(nsp_%s);\n" pname  pname  ^
  Printf.sprintf "  else if (nsp_%s != Py_None) {\n" pname  ^
  Printf.sprintf "      Scierror( \"Error: %s should be a file object or None\\n\");\n"  pname ^
  Printf.sprintf "      return RET_BUG;\n" ^
  Printf.sprintf "  }\n"
;;

let dflt pname  =
  Printf.sprintf "  if (nsp_%s)\n"  pname ^
  Printf.sprintf "      %s = PyFile_AsFile(nsp_%s);\n" pname  pname
;;

let file_arg_write_param _oname params info _byref=
  let ( varlist, code , pname, info ) =
    if params.pnull then
      let info = add_parselist info params.pvarargs "obj"  ["&nsp_" ^ params.pname]  [params.pname] in
      match params.pdflt with
      | None ->
	  let varlist1 = varlist_add info.varlist "FILE"  ("*" ^ params.pname ^ " = NULL") in
	  let varlist1 = varlist_add varlist1 "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL" ) in
	  (varlist1, null params.pname  , params.pname , info )
      | Some x ->
	  let varlist1 = varlist_add info.varlist "FILE"  ("*" ^ params.pname ^ " = " ^ x ) in
	  let varlist1 = varlist_add varlist1 "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	  (varlist1, nulldflt params.pname , params.pname , info )
    else
      match params.pdflt with
      | None ->
	let varlist1 = varlist_add info.varlist "NspObject"  ("*" ^ params.pname ^ " = NULL" ) in
	let info = add_parselist info params.pvarargs "obj_check"  ["&PyFile_Type"; ("&" ^ params.pname)]  [params.pname] in
	(varlist1, (Printf.sprintf "%s" params.pname), "PyFile_AsFile(" ^ params.pname ^ ")" , info)
      | Some x ->
	  let varlist1 = varlist_add info.varlist "FILE"  ("*" ^ params.pname ^ " = " ^ x) in
	  let varlist1 = varlist_add varlist1 "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	  (varlist1, dflt params.pname, params.pname, info) in
  { info with
    arglist = pname :: info.arglist;
    codebefore = code :: info.codebefore;
    varlist = varlist;
  }
;;

let file_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = file_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let file_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "FILE"  "*ret" in
  let codeafter = "  if (ret == NULL) return NULL;\n" ^
    "  return PyFile_FromFile(ret, \"\"  \"\"  fclose);\n" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let file_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "FILE"  "*ret" in
  let varlist = varlist_add varlist "NspObject"  "*nsp_ret" in
  let attrcodeafter = "  if (ret == NULL) return NULL;\n" ^
    "  return PyFile_FromFile(ret, \"\"  \"\"  fclose);\n" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let file_arg =
  { argtype with
    write_param = file_arg_write_param;
    attr_write_set = file_arg_attr_write_set;
    write_return = file_arg_write_return;
    attr_write_return = file_arg_attr_write_return ;
    (* attr_free_fields = file_arg_attr_free_fields ; *)
    (* attr_write_save = file_arg_attr_write_save ; *)
    (* attr_write_load = file_arg_attr_write_load ; *)
    (* attr_write_copy = file_arg_attr_write_copy ; *)
    (* attr_write_info = file_arg_attr_write_info ; *)
    (* attr_write_print = file_arg_attr_write_print ; *)
    (* attr_write_init = file_arg_attr_write_init ; *)
    (* attr_equal_fields = file_arg_attr_equal_fields ; *)
    (* attr_write_defval = file_arg_attr_write_defval ; *)
  }
;;

(* enum_arg: parameterized by typecode and enum_name *)

type enum_data = {
    enum_name: string;
    enum_typecode: string;
  }

let enum_arg_write_param enum_data _oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->  varlist_add info.varlist enum_data.enum_name params.pname
    | Some x -> varlist_add info.varlist enum_data.enum_name (params.pname ^ " = " ^ x) in
  let varlist = varlist_add varlist "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
  let enum typecode pname =
    Printf.sprintf "  if (nspg_enum_get_value(%s, nsp_%s, &%s)== FAIL)\n"
      typecode pname pname
    ^ "      return RET_BUG;\n" in
  let codebefore = enum enum_data.enum_typecode params.pname in
  let info = add_parselist info params.pvarargs "obj"  ["&nsp_" ^ params.pname]  [params.pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let enum_arg_attr_write_set enum_data oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = enum_arg_write_param enum_data  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let enum_arg_write_return  _enum_data _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "gint"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^  "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let enum_arg_attr_write_return _enum_data _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "gint"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let enum_arg_attr_write_defval _enum_data _objinfo _varname _params =
  (Printf.sprintf "")
;;

let make_enum_arg enum_data =
  { argtype with
    write_param = enum_arg_write_param enum_data;
    attr_write_set = enum_arg_attr_write_set enum_data;
    write_return = enum_arg_write_return enum_data;
    attr_write_return = enum_arg_attr_write_return  enum_data;
    (* attr_free_fields = enum_arg_attr_free_fields  enum_data; *)
    (* attr_write_save = enum_arg_attr_write_save  enum_data; *)
    (* attr_write_load = enum_arg_attr_write_load  enum_data; *)
    (* attr_write_copy = enum_arg_attr_write_copy  enum_data; *)
    (*attr_write_info = enum_arg_attr_write_info  enum_data;*)
    (*attr_write_print = enum_arg_attr_write_print  enum_data;*)
    (*attr_write_init = enum_arg_attr_write_init  enum_data;*)
    (*attr_equal_fields = enum_arg_attr_equal_fields  enum_data;*)
    attr_write_defval = enum_arg_attr_write_defval  enum_data;
  }
;;

(* flag_arg : parameterized by typecode and enum_name *)

let flag_arg_write_param flag_data _oname params info _byref=
  let varlist, default =
    match params.pdflt with
    | None -> ( varlist_add info.varlist  flag_data.enum_name params.pname, "")
    | Some x ->
	( varlist_add info.varlist flag_data.enum_name (params.pname ^ " = " ^ x),
	  (Printf.sprintf "nsp_%s && " params.pname)) in
  let flag default typecode pname =
    Printf.sprintf "  if (%snspg_flags_get_value(%s, nsp_%s, &%s)==FAIL)\n"
      default typecode pname pname
    ^ "      return RET_BUG;\n" in
  let varlist = varlist_add varlist "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
  let info = add_parselist info params.pvarargs "obj"  ["&nsp_" ^ params.pname]  [params.pname] in
  let codebefore = flag default flag_data.enum_typecode params.pname  in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let flag_arg_attr_write_set flag_data oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = flag_arg_write_param flag_data  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let flag_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "guint"  "ret" in
  let codeafter = "  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n" ^   "  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let flag_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "guint"  "ret" in
  let attrcodeafter = "  return nsp_new_double_obj((double) ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let flag_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let make_flag_arg flag_data =
  { argtype with
    write_param = flag_arg_write_param flag_data;
    attr_write_set = flag_arg_attr_write_set flag_data;
    write_return = flag_arg_write_return;
    attr_write_return = flag_arg_attr_write_return ;
    attr_write_defval = flag_arg_attr_write_defval ;
  }
;;

(* NspGenericArg nsp_generic_arg_
 * ------------------------------ *)

type nsp_generic_data =
    {
     ng_name: string;
     ng_fullname: string;
     ng_nsp_arg_type: string;
     ng_shortname: string;
     ng_shortname_uc: string;
   }

let init_nsp_generic_data fullname name shortname nsp_arg_type =
  {
   ng_name = name;
   ng_fullname = fullname;
   ng_shortname = shortname;
   ng_shortname_uc = (String.uppercase_ascii shortname);
   ng_nsp_arg_type = nsp_arg_type;
 }
;;

let nsp_generic_arg_write_param nsp_generic_data oname params info byref=
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist nsp_generic_data.ng_fullname ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist nsp_generic_data.ng_fullname ("*" ^ params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs nsp_generic_data.ng_nsp_arg_type ["&" ^ params.pname]  [params.pname] in
  let attrcodebefore =
    (Printf.sprintf "  if ( ! Is%s(O) ) return FAIL;\n"  nsp_generic_data.ng_shortname )
    ^ (Printf.sprintf "  if ((%s = (%s *) nsp_object_copy_and_name(attr,O)) == NULL%s) return FAIL;\n"
	 params.pname nsp_generic_data.ng_fullname nsp_generic_data.ng_shortname_uc)
    ^ (
      if byref then
	( (Printf.sprintf "  if (((%s *) self)->obj->%s != NULL ) \n" oname params.pname)  ^
	  (Printf.sprintf "    nsp_%s_destroy(((%s *) self)->obj->%s);\n"
	     ( String.lowercase_ascii nsp_generic_data.ng_name) oname params.pname))
    else
	( (Printf.sprintf "  if (((%s *) self)->%s != NULL ) \n"  oname params.pname)
	  ^ (Printf.sprintf  "  nsp_%s_destroy(((%s *) self)->%s);\n"
	       (String.lowercase_ascii nsp_generic_data.ng_name) oname params.pname))) in
  let codebefore = "" in
    (*
      ^ (Printf.sprintf "/*  %s << size %s*/\n" params.pname psize)
      ^ (Printf.sprintf "/* %s << %d */\n"  params.pname pos)
     *)
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
    attrcodebefore = attrcodebefore :: info.attrcodebefore ;
    setobj = true;
  }
;;

let nsp_generic_arg_attr_write_set nsp_generic_data oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_generic_arg_write_param nsp_generic_data  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_generic_arg_write_return nsp_generic_data _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist nsp_generic_data.ng_fullname "*ret" in
  let codeafter= "  if ( ret == NULL" ^ nsp_generic_data.ng_shortname_uc ^ ") return RET_BUG;\n" ^
    "  MoveObj(stack,1,NSP_OBJECT(ret));\n" ^
    "  return 1;" in
  { info with varlist = varlist ;
    codeafter = codeafter :: info.codeafter ;}
;;

let nsp_generic_arg_attr_write_return nsp_generic_data _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist nsp_generic_data.ng_fullname "*ret" in
  let attrcodeafter = "  return (NspObject *) ret;" in
  { info with varlist = varlist ;setobj = true;
    attrcodeafter = attrcodeafter :: info.attrcodeafter ;}

;;
let nsp_generic_arg_attr_write_save _nsp_generic_data varname params _byref=
  (Printf.sprintf
     "  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(%s->%s)) == FAIL) return FAIL;\n"
     varname params.pname )
;;

let nsp_generic_arg_attr_write_load nsp_generic_data varname params _byref=
  (Printf.sprintf "  if ((%s->%s =(%s *) nsp_object_xdr_load(xdrs))== NULL%s) return NULL;\n"
     varname params.pname nsp_generic_data.ng_fullname nsp_generic_data.ng_shortname_uc)
;;

let nsp_generic_arg_attr_write_copy  nsp_generic_data _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (* # this part is used in copy or full_copy  *)
    (Printf.sprintf "  if ( %s->%s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n"
       right_varname params.pname left_varname params.pname)
    ^ (Printf.sprintf "      if ((%s->%s = (%s *) %s_and_name(\"%s\", NSP_OBJECT(%s->%s))) == NULL%s) return NULL;\n    }\n"
	 left_varname params.pname nsp_generic_data.ng_fullname _f_copy_name  params.pname right_varname params.pname nsp_generic_data.ng_shortname_uc )
  else
    (* # this part is only used on create and we do not want to copy objects.  *)
    (* (Printf.sprintf "  if ( %s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n" params.pname left_varname params.pname)
       ^ (Printf.sprintf "      if ((%s->%s = (%s * )  zzz%s_and_name("%s" NSP_OBJECT(%s))) == NULL%s) return NULL;\n    }\n" \
       left_varname params.pname _nsp_generic_data.ng_fullname _f_copy_name params.pname params.pname _nsp_generic_data.ng_shortname_uc) *)
    (Printf.sprintf "  %s->%s= %s;\n" left_varname params.pname params.pname )
;;

let nsp_generic_arg_attr_write_info _nsp_generic_data  _ptype pname _varname _byref =
  (Printf.sprintf "  nsp_object_info(NSP_OBJECT(%s->%s),indent+2,\"%s\" rec_level+1);\n" _varname pname pname)
;;

let nsp_generic_arg_attr_write_print _nsp_generic_data _objinfo print_mode varname params =
  let arg = if print_mode = "latex" then "FALSE" else "indent+2" in
  Printf.sprintf
    "  if ( %s->%s != NULL)\n    { if ( nsp_object_%s(NSP_OBJECT(%s->%s),%s,\"%s\", rec_level+1)== FALSE ) return FALSE ;\n    }\n"
    varname params.pname print_mode varname params.pname arg params.pname
;;

let nsp_generic_arg_attr_write_init  nsp_generic_data _objinfo varname params =
    (Printf.sprintf "  %s->%s = NULL%s;\n" varname params.pname nsp_generic_data.ng_shortname_uc);
;;

let nsp_generic_arg_attr_free_fields nsp_generic_data  _ptype pname varname byref =
  let ind = if byref then  "  " else "" in
  ind ^ (Printf.sprintf "  if ( %s->%s != NULL ) \n"  varname pname )
  ^ ind ^ (Printf.sprintf "    nsp_%s_destroy(%s->%s);\n"
	     (String.lowercase_ascii nsp_generic_data.ng_name) varname pname)
;;

let nsp_generic_arg_attr_equal_fields _nsp_generic_data objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "  if ( NSP_OBJECT(A->%s)->type->eq(A->%s,loc->%s) == FALSE ) return FALSE;\n"
     pname pname pname)
;;

let nsp_generic_arg_attr_write_defval  nsp_generic_data objinfo varname params =
  let s_byref = if objinfo.or_byref then "ref" else "non ref" in
  (Printf.sprintf "/* defvalue: %s %s %s %s */\n"
     nsp_generic_data.ng_fullname  nsp_generic_data.ng_shortname
     nsp_generic_data.ng_shortname_uc nsp_generic_data.ng_nsp_arg_type)
  ^ (Printf.sprintf "/* [defvalue for %s %s %s]*/ \n" params.pname varname s_byref)
;;

let make_nsp_generic_arg nsp_generic_data =
  { argtype with
    write_param = nsp_generic_arg_write_param nsp_generic_data ;
    attr_write_set = nsp_generic_arg_attr_write_set nsp_generic_data ;
    write_return = nsp_generic_arg_write_return nsp_generic_data ;
    attr_write_return = nsp_generic_arg_attr_write_return  nsp_generic_data ;
    attr_free_fields = nsp_generic_arg_attr_free_fields  nsp_generic_data ;
    attr_write_save = nsp_generic_arg_attr_write_save  nsp_generic_data ;
    attr_write_load = nsp_generic_arg_attr_write_load  nsp_generic_data ;
    attr_write_copy = nsp_generic_arg_attr_write_copy  nsp_generic_data ;
    attr_write_info = nsp_generic_arg_attr_write_info  nsp_generic_data ;
    attr_write_print = nsp_generic_arg_attr_write_print  nsp_generic_data ;
    attr_write_init = nsp_generic_arg_attr_write_init  nsp_generic_data ;
    attr_equal_fields = nsp_generic_arg_attr_equal_fields  nsp_generic_data ;
    attr_write_defval = nsp_generic_arg_attr_write_defval  nsp_generic_data ;
    (*
       attr_write_field_declaration = nsp_generic_arg_attr_write_field_declaration;
       attr_write_create_call = nsp_generic_arg_attr_write_create_call;
     *)
  };;

(* nsp_generic_arg_mat *)

let nsp_generic_arg_mat_attr_write_defval  nsp_generic_data _objinfo varname params =
  (* # the following code is used to give a default value for a NspMatrix  *)
  let size, defi =
    match params.pdflt with
    | None -> ( "0,0",  "")
    | Some x ->
	((Printf.sprintf "1,%s" params.psize) ,
	 (Printf.sprintf "   double x_def[%s]=%s;\n" params.psize x)) in
  (Printf.sprintf"  if ( %s->%s == NULL%s) \n    {\n  %s"
     varname params.pname nsp_generic_data.ng_shortname_uc defi)
  ^ (Printf.sprintf"     if (( %s->%s = nsp_matrix_create(\"%s\",'r',%s)) == NULL%s)\n       return FAIL;\n"
       varname params.pname params.pname size nsp_generic_data.ng_shortname_uc)
  ^ (
    match params.pdflt with
    | None -> (Printf.sprintf "\n    }\n" )
    | Some _x ->
	(Printf.sprintf "      memcpy(%s->%s->R,x_def,%s*sizeof(double));\n  }\n"
	   varname params.pname  params.psize)
   )
;;

let nsp_generic_arg_mat_attr_write_init  nsp_generic_data _objinfo varname params=
  (Printf.sprintf "  %s->%s = NULL%s;\n" varname params.pname nsp_generic_data.ng_shortname_uc);
;;

let make_nsp_generic_arg_mat nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_init = nsp_generic_arg_mat_attr_write_init nsp_generic_data ;
    attr_write_defval = nsp_generic_arg_mat_attr_write_defval nsp_generic_data ;
  }
;;

(* nsp_generic_arg_bmat *)

let nsp_generic_arg_bmat_attr_write_defval nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf
       "     if (( %s->%s = nsp_bmatrix_create(\"%s\",0,0)) == NULL%s)\n       return FAIL;\n    }\n"
       varname params.pname params.pname nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_bmat nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_bmat_attr_write_defval  nsp_generic_data ;
  }
;;

(* nsp_generic_arg_smat *)

let nsp_generic_arg_smat_attr_write_defval  nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf
       "     if (( %s->%s = nsp_smatrix_create(\"%s\",0,0,\"v\",0)) == NULL%s)\n       return FAIL;\n    }\n"
       varname params.pname params.pname nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_smat nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_smat_attr_write_defval nsp_generic_data ;
 }
;;
(* nsp_generic_arg_pmat *)

let nsp_generic_arg_pmat_attr_write_defval  nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf
       "     if (( %s->%s = nsp_pmatrix_create(\"%s\",0,0,NULL,-1,NULL)) == NULL%s)\n       return FAIL;\n    }\n"
       varname params.pname params.pname nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_pmat nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_pmat_attr_write_defval nsp_generic_data ;
 }
;;

(* nsp_generic_arg_list *)

let nsp_generic_arg_list_attr_write_defval  nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf "     if (( %s->%s = nsp_list_create(\"%s\")) == NULL%s)\n       return FAIL;\n    }\n"
       varname params.pname params.pname nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_list nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_list_attr_write_defval nsp_generic_data ;
 }
;;

(* nsp_generic_arg_spcol *)

let nsp_generic_arg_spcol_attr_write_defval  nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf "     if (( %s->%s = nsp_spcolmatrix_create(\"%s\",'r',0,0) ) == NULL%s)\n       return FAIL;\n    }\n"
       varname params.pname params.pname  nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_spcol nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_spcol_attr_write_defval nsp_generic_data ;
 }
;;

(* nsp_generic_arg_cells *)

let nsp_generic_arg_cells_attr_write_defval  nsp_generic_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname  nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf  "     if (( %s->%s = nsp_cells_create(\"%s\" 0,0) ) == NULL%s)\n       return FAIL;\n    }\n"
      varname params.pname params.pname  nsp_generic_data.ng_shortname_uc)
;;

let make_nsp_generic_arg_cells nsp_generic_data =
  let arg = make_nsp_generic_arg nsp_generic_data in
  { arg with
    attr_write_defval = nsp_generic_arg_cells_attr_write_defval nsp_generic_data ;
 }
;;

(* nsp_mat_arg *)

let nsp_mat_arg_write_param_gen params info nsp_type _byref =
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs nsp_type ["&" ^ params.pname]  [params.pname] in
  let codebefore = (Printf.sprintf "  if ((%s = (NspMatrix *) nsp_object_copy(O)) == NULLMAT) return FAIL;\n"
		       params.pname)
    ^ (Printf.sprintf "/* %s << size %s*/\n"  params.pname params.psize) in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }

;;
let nsp_mat_arg_write_param _oname params info byref=
  nsp_mat_arg_write_param_gen params info "mat" byref
;;

let nsp_mat_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_mat_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_mat_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist "NspMatrix"  "*ret" in
  let codeafter = ("  if ( ret == NULLMAT) return RET_BUG;\n" ^
                          "  MoveObj(stack,1,NSP_OBJECT(ret));\n" ^
                          "  return 1;") in
  { info with
    (* arglist = pname :: info.arglist; *)
    varlist = varlist ;
    codeafter = codeafter :: info.codeafter ;
  }
;;

let nsp_mat_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "NspMatrix"  "*ret" in
  let attrcodeafter = "  return ret;" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_mat_arg =
  {argtype with
   write_param = nsp_mat_arg_write_param;
   attr_write_set = nsp_mat_arg_attr_write_set;
   write_return = nsp_mat_arg_write_return;
   attr_write_return = nsp_mat_arg_attr_write_return ;
 }
;;

(* nsp_mat_copy_arg *)

let nsp_mat_copy_arg_write_param _oname params info byref=
  nsp_mat_arg_write_param_gen params info  "matcopy" byref
;;

let nsp_mat_copy_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_mat_copy_arg_write_param  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_mat_copy_arg =
  { nsp_mat_arg with
    write_param = nsp_mat_copy_arg_write_param;
    attr_write_set = nsp_mat_copy_arg_attr_write_set;
  }
;;

(* nsp_double_array_arg "double[]" : arguments are expected to be real matrices *)

let nsp_double_array_arg_write_param_gen params info nsp_type _byref =
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "zzdouble"  ("*" ^ params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs nsp_type ["&" ^ params.pname]  [params.pname] in
  (* this is useless since the matrix is obtained with the nsp_type set to "realmat" 
    let codebefore =
    Printf.sprintf "  if ( ! IsMat(%s) " params.pname ^
    Printf.sprintf "  || ((NspMatrix *) %s)->rc_type != 'r' " params.pname ^ 
    (if params.psize = "" then 
       ")\n" 
     else
       Printf.sprintf " || ((NspMatrix *) %s)->mn != [%s] ) \n" params.pname params.psize) ^
    Printf.sprintf "     return FAIL;\n" in 
   *)
  let codebefore = "" in 
  (*   Printf.sprintf "  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(double));\n" params.pname params.psize in *)
  { info with
    arglist = ( params.pname ^ "->R")  :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_double_array_arg_write_param _oname params info byref=
  nsp_double_array_arg_write_param_gen params info "realmat" byref
;;

let nsp_double_array_arg_attr_write_set oname params info byref=
  let varlist = varlist_add info.varlist "NspMatrix"  "*M=(NspMatrix *) O" in
  let pset_name = pset_name_set byref oname params.pname in
  let codebefore =
    Printf.sprintf   "  if ( ! IsMat(O) || M->rc_type != 'r' || M->mn != %s ) \n" params.psize ^
    Printf.sprintf   "     return FAIL;\n" ^
    Printf.sprintf   "  Mat2double(M);\n" ^
    Printf.sprintf   "  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(double));\n" pset_name params.psize in
  { info with
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_double_array_arg_attr_write_init _objinfo varname params =
  let vdef =
    match params.pdflt with
    | None -> "{0}"
    | Some x -> x  in
  Printf.sprintf "  {\n" ^
  Printf.sprintf "    double x_def[%s]=%s;\n" params.psize vdef ^
  Printf.sprintf "    memcpy(%s->%s,x_def,%s*sizeof(double));\n"  varname params.pname  params.psize ^
  Printf.sprintf "  }\n"
;;

let nsp_double_array_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  Printf.sprintf "  {\n" ^
  Printf.sprintf "    int i;\n" ^
  Printf.sprintf "    for ( i = 0 ; i < %s ; i++ )\n" params.psize ^
  Printf.sprintf "      if ( A->%s[i] != loc->%s[i] ) return FALSE;\n"  pname pname ^
  Printf.sprintf "  }\n"
;;

let nsp_double_array_arg_attr_write_save _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_save_array_d(xdrs,M->%s,%s)  == FAIL) return FAIL;\n"
     pname params.psize)
;;

let nsp_double_array_arg_attr_write_load _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_load_array_d(xdrs,M->%s,%s) == FAIL) return NULL;\n"  pname  params.psize)
;;

let nsp_double_array_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")

let nsp_double_array_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  memcpy("
    ^ left_varname ^ "->"^ params.pname ^","^right_varname ^"->"^ params.pname ^","^ params.psize^ "*sizeof(double));\n"
  else
    "  memcpy(" ^ left_varname ^ "->"^ params.pname ^","^ params.pname ^","^ params.psize^ "*sizeof(double));\n"
;;

let nsp_double_array_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

(* _ptype pname _varname _byref _print_mode _pdef params.psize _pcheck = *)

let nsp_double_array_arg_attr_write_print _objinfo print_mode varname params =
  let tag = if print_mode = "latex" then "latex_"  else "" in
  Printf.sprintf "  if ( nsp_print_%sarray_double(indent+2,\"%s\",%s->%s,%s,rec_level) == FALSE ) return FALSE ;\n"
    tag  params.pname varname params.pname params.psize
;;

let nsp_double_array_arg_attr_write_return _objinfo _ownsreturn params info=
  let varlist = varlist_add  info.varlist "double*"  "ret" in
  let attrcodeafter = (Printf.sprintf "  return NSP_OBJECT(nsp_matrix_create_from_array(NVOID,1,%s,ret,NULL));\n"  params.psize ) in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_double_array_arg =
  { nsp_mat_arg with
    write_param = nsp_double_array_arg_write_param;
    attr_write_set = nsp_double_array_arg_attr_write_set;
    (* write_return = nsp_double_array_arg_write_return; *)
   attr_write_return = nsp_double_array_arg_attr_write_return ;
   attr_free_fields = nsp_double_array_arg_attr_free_fields ;
   attr_write_save = nsp_double_array_arg_attr_write_save ;
   attr_write_load = nsp_double_array_arg_attr_write_load ;
   attr_write_copy = nsp_double_array_arg_attr_write_copy ;
    (* attr_write_info = nsp_double_array_arg_attr_write_info ; *)
   attr_write_print = nsp_double_array_arg_attr_write_print ;
   attr_write_init = nsp_double_array_arg_attr_write_init ;
   attr_equal_fields = nsp_double_array_arg_attr_equal_fields ;
   attr_write_defval = nsp_double_array_arg_attr_write_defval ;
 }
;;

(* nsp_double_array_copy_arg *)

let nsp_double_array_copy_arg_write_param _oname params info byref=
  nsp_double_array_arg_write_param_gen params info "matcopy" byref
;;

let nsp_double_array_copy_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_double_array_copy_arg_write_param  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_double_array_copy_arg =
  { nsp_double_array_arg with
    write_param = nsp_double_array_copy_arg_write_param;
    attr_write_set = nsp_double_array_copy_arg_attr_write_set;
  }
;;

(* nsp_complex_array_arg *)

let nsp_complex_array_arg_write_param_gen  params info nsp_type _byref =
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "zzdouble"  ("*" ^ params.pname ^ " = " ^ x ) in
  let info = add_parselist info params.pvarargs nsp_type  ["&" ^ params.pname]  [params.pname] in
  (* we only need to check that the matrix is comples since the matrix is obtained with the nsp_type set to "mat" *)
  let codebefore =
    Printf.sprintf "  if ((NspMatrix *) %s)->rc_type != \"c\" ) return FAIL;\n" params.pname  in 
  { info with
    arglist = ( params.pname ^ "->C")  :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_complex_array_arg_write_param _oname params info byref=
  nsp_complex_array_arg_write_param_gen params info "mat" byref
;;

let nsp_complex_array_arg_attr_write_set oname params info byref=
  let varlist = varlist_add info.varlist "NspMatrix"  "*M=(NspMatrix *) O" in
  let pset_name = pset_name_set byref oname params.pname in
  let codebefore =
    Printf.sprintf "  if ( ! IsMat(O) || M->rc_type != \"c\" || M->mn != %s ) \n" params.psize ^
    Printf.sprintf "     return FAIL;\n" ^
    Printf.sprintf "  Mat2double(M);\n" ^
    Printf.sprintf "  memcpy(%s, ((NspMatrix *) O)->C,%s*sizeof(doubleC));\n" pset_name params.psize in
  { info with
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_complex_array_arg_attr_write_init _objinfo varname params =
  let vdef =
    match params.pdflt with
    | None -> "{0}"
    | Some x -> x  in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    doubleC x_def[%s]=%s;\n" params.psize vdef  ^
  Printf.sprintf"    memcpy(%s->%s,x_def,%s*sizeof(doubleC));\n"  varname params.pname  params.psize  ^
  Printf.sprintf"  }\n"
;;

let nsp_complex_array_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    int i;\n" ^
  Printf.sprintf"    for ( i = 0 ; i < %s ; i++ )\n" params.psize  ^
  Printf.sprintf"      if ( A->%s[i] != loc->%s[i] ) return FALSE;\n"  pname pname ^
  Printf.sprintf"  }\n"
;;

let nsp_complex_array_arg_attr_write_save _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_save_array_d(xdrs,M->%s,2*%s)  == FAIL) return FAIL;\n"
     pname params.psize)
;;

let nsp_complex_array_arg_attr_write_load _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_load_array_d(xdrs,M->%s,2*%s) == FAIL) return NULL;\n"  pname params.psize)
;;

let nsp_complex_array_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let nsp_complex_array_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^right_varname ^"->"^ params.pname ^","^ params.psize^ "*sizeof(doubleC));\n"
  else
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^ params.pname ^","^ params.psize^ "*sizeof(doubleC));\n"
;;

let nsp_complex_array_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let nsp_complex_array_arg_attr_write_print _objinfo print_mode varname params =
  let tag = if print_mode = "latex" then "latex_"  else "" in
  Printf.sprintf "  if ( ZZnsp_print_%sarray_double(indent+2,\"%s\",%s->%s,%s,rec_level) == FALSE ) return FALSE ;\n"
     tag  params.pname varname params.pname params.psize
;;

let nsp_complex_array_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "doubleC*"  "ret" in
  let attrcodeafter = "  return ZZ NSP_OBJECT(nsp_matrix_create_from_array(NVOID,1,%s,ret,NULL));\n" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_complex_array_arg =
  { nsp_mat_arg with
    write_param = nsp_complex_array_arg_write_param;
    attr_write_set = nsp_complex_array_arg_attr_write_set;
    (* write_return = nsp_complex_array_arg_write_return;  *)
    attr_write_return = nsp_complex_array_arg_attr_write_return ;
    attr_free_fields = nsp_complex_array_arg_attr_free_fields ;
    attr_write_save = nsp_complex_array_arg_attr_write_save ;
    attr_write_load = nsp_complex_array_arg_attr_write_load ;
    attr_write_copy = nsp_complex_array_arg_attr_write_copy ;
    (* attr_write_info = nsp_complex_array_arg_attr_write_info ; *)
    attr_write_print = nsp_complex_array_arg_attr_write_print ;
    attr_write_init = nsp_complex_array_arg_attr_write_init ;
    attr_equal_fields = nsp_complex_array_arg_attr_equal_fields ;
    attr_write_defval = nsp_complex_array_arg_attr_write_defval ;
 }
;;


(* nsp_complex_array_copy_arg *)

let nsp_complex_array_copy_arg_write_param _oname params info byref=
  nsp_complex_array_arg_write_param_gen params info  "matcopy" byref
;;

let nsp_complex_array_copy_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_complex_array_copy_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_complex_array_copy_arg =
  { nsp_complex_array_arg with
    write_param = nsp_complex_array_copy_arg_write_param;
    attr_write_set = nsp_complex_array_copy_arg_attr_write_set;
 }
;;

(* nsp_bool_array_arg *)

let nsp_bool_array_arg_write_param_gen params info nsp_type _byref =
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspBMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "zzint"  ("*" ^ params.pname ^ " = " ^ x) in
  let codebefore =
    Printf.sprintf"  if ( ! IsBMat(O) \n"  ^
    Printf.sprintf"       || ((NspBMatrix *) O)->mn != %s ) \n" params.psize ^
    Printf.sprintf"     return FAIL;\n" ^
    Printf.sprintf"  memcpy(%s, ((NspBMatrix *) O)->B,%s*sizeof(int));\n" params.pname params.psize in
  let info = add_parselist info params.pvarargs nsp_type  ["&" ^ params.pname]  [params.pname] in
  { info with
    arglist = ( params.pname ^ "->B")  :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_bool_array_arg_write_param _oname params info byref=
  nsp_bool_array_arg_write_param_gen params info "bmat" byref
;;

let nsp_bool_array_arg_attr_write_set oname params info byref=
  let varlist = varlist_add info.varlist "NspBMatrix"  "*M=(NspBMatrix *) O" in
  let pset_name = pset_name_set byref oname params.pname in
  let codebefore =
    Printf.sprintf "  if ( ! IsBMat(O) || M->mn != %s ) \n"  params.psize ^
    Printf.sprintf "     return FAIL;\n" ^
    Printf.sprintf "  memcpy(%s, ((NspBMatrix *) O)->B,%s*sizeof(int));\n" pset_name params.psize in
  { info with
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_bool_array_arg_attr_write_init _objinfo varname params =
  let vdef =
    match params.pdflt with
    | None -> "{0}"
    | Some x -> x  in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    int x_def[%s]=%s;\n" params.psize vdef  ^
  Printf.sprintf"    memcpy(%s->%s,x_def,%s*sizeof(int));\n" varname params.pname params.psize ^
  Printf.sprintf"  }\n"
;;

let nsp_bool_array_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    int i;\n" ^
  Printf.sprintf"    for ( i = 0 ; i < %s ; i++ )\n"  params.psize ^
  Printf.sprintf"      if ( A->%s[i] != loc->%s[i] ) return FALSE;\n"  pname pname ^
  Printf.sprintf"  }\n"
;;

let nsp_bool_array_arg_attr_write_save _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_save_array_i(xdrs,M->%s,%s)  == FAIL) return FAIL;\n"
     pname  params.psize)
;;

let nsp_bool_array_arg_attr_write_load _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_load_array_i(xdrs,M->%s,%s) == FAIL) return NULL;\n"  pname  params.psize)
;;

let nsp_bool_array_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let nsp_bool_array_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^right_varname ^"->"^ params.pname ^","^ params.psize^ "*sizeof(int));\n"
  else
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^ params.pname ^","^ params.psize^ "*sizeof(int));\n"
;;

let nsp_bool_array_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")
;;

let nsp_bool_array_arg_attr_write_print _objinfo print_mode varname params =
  let tag = if print_mode = "latex" then "latex_"  else "" in
  Printf.sprintf
    "  if ( nsp_print_%sarray_int(indent+2,\"%s\",%s->%s,%s,rec_level) == FALSE ) return FALSE ;\n"
    tag  params.pname varname params.pname params.psize
;;

let nsp_bool_array_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "int*"  "ret" in
  let attrcodeafter ="zz  return NSP_OBJECT(nsp_matrix_create_from_array(NVOID,1,%s,ret,NULL));\n" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_bool_array_arg =
  { nsp_mat_arg with
    write_param = nsp_bool_array_arg_write_param;
    attr_write_set = nsp_bool_array_arg_attr_write_set;
    (* write_return = nsp_bool_array_arg_write_return;  *)
    attr_write_return = nsp_bool_array_arg_attr_write_return ;
    attr_free_fields = nsp_bool_array_arg_attr_free_fields ;
    attr_write_save = nsp_bool_array_arg_attr_write_save ;
    attr_write_load = nsp_bool_array_arg_attr_write_load ;
    attr_write_copy = nsp_bool_array_arg_attr_write_copy ;
    (* attr_write_info = nsp_bool_array_arg_attr_write_info ; *)
    attr_write_print = nsp_bool_array_arg_attr_write_print ;
    attr_write_init = nsp_bool_array_arg_attr_write_init ;
    attr_equal_fields = nsp_bool_array_arg_attr_equal_fields ;
    attr_write_defval = nsp_bool_array_arg_attr_write_defval ;
 }
;;

(* nsp_bool_array_copy_arg *)

let nsp_bool_array_copy_arg_write_param _oname params info byref=
  nsp_bool_array_arg_write_param_gen params info "matcopy" byref
;;

let nsp_bool_array_copy_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_bool_array_copy_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}

let nsp_bool_array_copy_arg =
  { nsp_bool_array_arg with
    write_param = nsp_bool_array_copy_arg_write_param;
    attr_write_set = nsp_bool_array_copy_arg_attr_write_set;
  }
;;

(* nsp_int_array_arg *)

let nsp_int_array_arg_write_param_gen  params info nsp_type _byref =
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspMatrix"  ("*" ^ params.pname)
    | Some x ->
	varlist_add info.varlist "zzint"  ("*" ^ params.pname ^ " = " ^ x)  in
  let info = add_parselist info params.pvarargs nsp_type  ["&" ^ params.pname]  [params.pname] in
  let codebefore =
    Printf.sprintf"  if ( ! IsMat(O) \n" ^
    Printf.sprintf"       || ((NspMatrix *) O)->rc_type != 'r' \n" ^
    Printf.sprintf"       || ((NspMatrix *) O)->mn != %s ) \n" params.psize ^
    Printf.sprintf"     return FAIL;\n" ^
    Printf.sprintf"  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(int));\n" params.pname params.psize in
  { info with
    arglist = ( params.pname ^ "->I")  :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_int_array_arg_write_param _oname params info byref=
  nsp_int_array_arg_write_param_gen params info "mat_int" byref
;;

let nsp_int_array_arg_attr_write_set oname params info byref=
  let varlist = varlist_add info.varlist "NspMatrix"  "*M=(NspMatrix *) O" in
  let pset_name = pset_name_set byref oname params.pname in
  let codebefore =
    Printf.sprintf "  if ( ! IsMat(O) || M->rc_type != 'r' || M->mn != %s ) \n" params.psize ^
    Printf.sprintf"     return FAIL;\n" ^
    Printf.sprintf"  Mat2double(M);\n" ^
    Printf.sprintf"  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(int));\n" pset_name params.psize in
  { info with
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_int_array_arg_attr_write_init _objinfo varname params =
  let vdef =
    match params.pdflt with
    | None -> "{0}"
    | Some x -> x  in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    int x_def[%s]=%s;\n" params.psize vdef ^
  Printf.sprintf"    memcpy(%s->%s,x_def,%s*sizeof(int));\n" varname params.pname params.psize ^
  Printf.sprintf"  }\n"
;;

let nsp_int_array_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  Printf.sprintf"  {\n" ^
  Printf.sprintf"    int i;\n" ^
  Printf.sprintf"    for ( i = 0 ; i < %s ; i++ )\n"    params.psize ^
  Printf.sprintf"      if ( A->%s[i] != loc->%s[i] ) return FALSE;\n"  pname pname ^
  Printf.sprintf"  }\n"
;;

let nsp_int_array_arg_attr_write_save _varname params _byref=
  let pname = if _byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_save_array_d(xdrs,M->%s,%s)  == FAIL) return FAIL;\n"
     pname params.psize)
;;

let nsp_int_array_arg_attr_write_load _varname params _byref=
  let pname = if _byref then "obj->"^params.pname else params.pname in
  (Printf.sprintf "  if ( nsp_xdr_load_array_d(xdrs,M->%s,%s) == FAIL) return NULL;\n"  pname  params.psize)
;;

let nsp_int_array_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")

let nsp_int_array_arg_attr_write_copy _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^right_varname ^"->"^ params.pname ^","^ params.psize^ "*sizeof(int));\n"
  else
    "  memcpy("^ left_varname ^ "->"^ params.pname ^","^ params.pname ^","^ params.psize ^ "*sizeof(int));\n"
;;

let nsp_int_array_arg_attr_write_defval _objinfo _varname _params =
  (Printf.sprintf "")

let nsp_int_array_arg_attr_write_print _objinfo print_mode varname params =
  let tag = if print_mode = "latex" then "latex_"  else "" in
  Printf.sprintf
    "  if ( nsp_print_%sarray_double(indent+2,\"%s\",%s->%s,%s,rec_level) == FALSE ) return FALSE ;\n"
    tag  params.pname varname params.pname params.psize
;;

let nsp_int_array_arg_attr_write_return _objinfo _ownsreturn params info=
  let varlist = varlist_add  info.varlist "int*"  "ret" in
  let attrcodeafter =
    (Printf.sprintf "  return NSP_OBJECT(nsp_matrix_create_from_array(NVOID,1,%s,ret,NULL));\n"
       params.psize ) in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_int_array_arg =
  { nsp_mat_arg with
    write_param = nsp_int_array_arg_write_param;
    attr_write_set = nsp_int_array_arg_attr_write_set;
    (* write_return = nsp_int_array_arg_write_return;   *)
    attr_write_return = nsp_int_array_arg_attr_write_return ;
    attr_free_fields = nsp_int_array_arg_attr_free_fields ;
    attr_write_save = nsp_int_array_arg_attr_write_save ;
    attr_write_load = nsp_int_array_arg_attr_write_load ;
    attr_write_copy = nsp_int_array_arg_attr_write_copy ;
    (* attr_write_info = nsp_int_array_arg_attr_write_info ; *)
    attr_write_print = nsp_int_array_arg_attr_write_print ;
    attr_write_init = nsp_int_array_arg_attr_write_init ;
    attr_equal_fields = nsp_int_array_arg_attr_equal_fields ;
    attr_write_defval = nsp_int_array_arg_attr_write_defval ;
 }
;;

(* nsp_int_array_copy_arg *)

let nsp_int_array_copy_arg_write_param _oname params info byref=
  nsp_int_array_arg_write_param_gen params info "matcopy" byref
;;

let nsp_int_array_copy_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_int_array_copy_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_int_array_copy_arg =
  { nsp_int_array_arg with
    write_param = nsp_int_array_copy_arg_write_param;
    attr_write_set = nsp_int_array_copy_arg_attr_write_set;
  }
;;

(* void_pointer_arg *)

let void_pointer_arg_write_param oname params info _byref=
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "void"  ("*" ^ params.pname)
    | Some "NULL" ->
	varlist_add info.varlist "void"  ("*" ^ params.pname ^ " = " ^ "NULL")
    | Some x ->
	let pdflt1 = "\"" ^ x ^ "\"" in
	varlist_add info.varlist "void"  ("*" ^ params.pname ^ " = " ^ pdflt1) in
  let info =
    if params.pnull then
      add_parselist info params.pvarargs "void*"  ["&" ^ params.pname]  [params.pname]
    else
      add_parselist info params.pvarargs "void*"  ["&" ^ params.pname]  [params.pname] in
  let codebefore = (Printf.sprintf "  if ((%s = nsp_string_object(O))==NULL) return FAIL;\n"  params.pname)
    ^ (Printf.sprintf "  if ((%s = nsp_string_copy(%s)) ==NULL) return FAIL;\n"  params.pname params.pname)
    ^ (Printf.sprintf "  nsp_string_destroy(&((%s *) self)->obj->%s);\n" oname params.pname) in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let void_pointer_arg_attr_write_set oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = void_pointer_arg_write_param oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let void_pointer_arg_write_return _ptype ownsreturn info  =
  let ( varlist, codeafter ) =
    if ownsreturn then
      (* # have to free result ... *)
      ( varlist_add info.varlist "void"  "*ret",
	"  if ( nsp_move_double(stack,1, ret)== FAIL) return RET_BUG;\n" ^
	"  return 1;")
    else
      ( varlist_add info.varlist "const gchar"  "*ret",
	"  if ( nsp_move_double(stack,1,ret )== FAIL) return RET_BUG;\n" ^
        "  return 1;") in
  { info with
    varlist = varlist ;
    codeafter = codeafter :: info.codeafter ;
  }
;;

let void_pointer_arg_attr_write_return _objinfo ownsreturn _params info=
  let varlist = varlist_add info.varlist "NspObject"  "*nsp_ret" in
  let ( varlist, attrcodeafter ) =
    if ownsreturn then
      ( varlist_add varlist "void"  "*ret",
	"  nsp_ret = nsp_new_double_obj(NVOID,ret);\n  return nsp_ret;")
    else
      ( varlist_add varlist "void"  "*ret",
	"  nsp_ret = nsp_new_double_obj(NVOID,ret);\n  return nsp_ret;") in
  { info with
    varlist = varlist ;
    attrcodeafter = attrcodeafter :: info.attrcodeafter ;
  }
;;

let void_pointer_arg_attr_free_fields _ptype _pname _varname _byref =
  (Printf.sprintf "")
;;

let void_pointer_arg_attr_write_save _varname _params _byref=
  (Printf.sprintf "")
;;

let void_pointer_arg_attr_write_load _varname params _byref=
  (Printf.sprintf "  %s->%s = NULL;\n"   _varname params.pname )
;;

let void_pointer_arg_attr_write_copy objinfo params left_varname right_varname _f_copy_name =
  let slotname = (Printf.sprintf "%s.%s" objinfo.or_name  params.pname ) in
  let cname = (String.lowercase_ascii objinfo.or_name); in
  if Overrides.is "override-field-void-pointer-copy" slotname then
    (* # user will have to provide a copy field function  *)
    let copy_fun = (Printf.sprintf "nsp_%s_%s_copy" cname  params.pname) in
    if right_varname <> "" then
      (Printf.sprintf "  %s->%s = %s( %s->%s );\n" left_varname params.pname copy_fun right_varname params.pname)
    else
      (Printf.sprintf "  %s->%s = %s(%s);\n" left_varname params.pname copy_fun params.pname)
  else
    if right_varname <> "" then
      (Printf.sprintf "  %s->%s = %s->%s;\n"  left_varname params.pname right_varname params.pname)
    else
      (Printf.sprintf "  %s->%s = %s;\n" left_varname params.pname params.pname )
;;

let void_pointer_arg_attr_write_info _ptype pname _varname _byref =
  (Printf.sprintf "  Sciprintf1(indent+2,\"%s=0x%%x\\n\" %s->%s);\n"  pname _varname pname)
;;

let void_pointer_arg_attr_write_print _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf "  Sciprintf1(indent+2,\"\\\\verb|%s|= \\\\verb@0x%%x@\\n\",%s->%s);\n"
      params.pname varname params.pname
  else
    Printf.sprintf "  Sciprintf1(indent+2,\"%s=0x%%x\\n\", %s->%s);\n"
      params.pname varname params.pname
;;

let void_pointer_arg_attr_write_init _objinfo varname params=
  (Printf.sprintf "  %s->%s = NULL;\n"  varname params.pname )
;;

let void_pointer_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname)
;;

let void_pointer_arg_attr_write_defval _objinfo _varname _params =
  Printf.sprintf ""
;;

let void_pointer_arg =
  { argtype with
    write_param = void_pointer_arg_write_param;
    attr_write_set = void_pointer_arg_attr_write_set;
    write_return = void_pointer_arg_write_return;
    attr_write_return = void_pointer_arg_attr_write_return ;
    attr_free_fields = void_pointer_arg_attr_free_fields ;
    attr_write_save = void_pointer_arg_attr_write_save ;
    attr_write_load = void_pointer_arg_attr_write_load ;
    attr_write_copy = void_pointer_arg_attr_write_copy ;
    attr_write_info = void_pointer_arg_attr_write_info ;
    attr_write_print = void_pointer_arg_attr_write_print ;
    attr_write_init = void_pointer_arg_attr_write_init ;
    attr_equal_fields = void_pointer_arg_attr_equal_fields ;
    attr_write_defval = void_pointer_arg_attr_write_defval ;
 }
;;

(* object_arg: used when a NspObject is used as attribute or argument.
 *------------------------------------------------------------------
 *)

type object_data =
    {
     od_objname: string;
     od_name: string;
     od_cast: string;
     od_parent: string;
     od_nsp_arg_type: string;
     od_shortname: string;
     od_shortname_uc: string;
   }

(*
let init_object_data objname name parent typecode =
  {
   od_objname = objname;
   od_name = name;
   od_cast = objname;
   od_parent = parent;
   od_nsp_arg_type = typecode;
   od_shortname = name;
   od_shortname_uc = (String.uppercase_ascii name);
 }
*)

let nulldflt sname stype scast =
  Printf.sprintf"  if ( nsp_%s != NULL ) {\n" sname ^
  Printf.sprintf"    if ( Is%s((NspObject *)nsp_%s))\n" stype sname  ^
  Printf.sprintf"      %s = %s(nsp_%s->obj);\n" sname scast sname ^
  Printf.sprintf"    else if (! IsNone((NspObject *)nsp_%s)) {\n" sname ^
  Printf.sprintf"         Scierror( \"Error: %s should be a %s or None\\n\");\n" sname stype ^
  Printf.sprintf"         return RET_BUG;\n" ^
  Printf.sprintf"    }\n" ^
  Printf.sprintf"  }\n"
;;

let null sname stype scast =
  Printf.sprintf"  if ( Is%s((NspObject *)nsp_%s))\n" stype sname ^
  Printf.sprintf"      %s = %s(nsp_%s->obj);\n" sname scast sname ^
  Printf.sprintf"  else if ( ! IsNone((NspObject *) nsp_%s))  {\n" sname ^
  Printf.sprintf"      Scierror( \"Error: %s should be a %s or None\\n\");\n" sname stype ^
  Printf.sprintf"      return RET_BUG;\n" ^
  Printf.sprintf"  }\n"
;;

let dflt sname scast =
  Printf.sprintf"  if (nsp_%s)\n" sname ^
  Printf.sprintf"      %s = %s(nsp_%s->obj);\n" sname scast sname
;;

(* XXXX : the default case should be improved, to remmove many special cases *)

let cast_name object_data =
  match object_data.od_cast with
  | "GObject" -> "G_OBJECT"
  | "GdkBitmap" -> "GDK_DRAWABLE"
  | "GdkDrawable" -> "GDK_DRAWABLE"
  | "GdkImage" -> "GDK_IMAGE"
  | "GdkPixbuf" -> "GDK_PIXBUF"
  | "GdkPixmap" -> "GDK_PIXMAP"
  | "GdkScreen" -> "GDK_SCREEN"
  | "GdkWindow" -> "GDK_WINDOW"
  | "GtkAdjustment" -> "GTK_ADJUSTMENT"
  | "GtkCellRenderer" -> "GTK_CELL_RENDERER"
  | "GtkEntryCompletion" -> "GTK_ENTRY_COMPLETION"
  | "GtkStyle" -> "GTK_STYLE"
  | "GtkTextBuffer" -> "GTK_TEXT_BUFFER"
  | "GtkTextTag" -> "GTK_TEXT_TAG"
  | "GtkTextTagTable" -> "GTK_TEXT_TAG_TABLE"
  | "GtkToolItem" -> "GTK_TOOL_ITEM"
  | "GtkTreeModel" -> "GTK_TREE_MODEL"
  | "GtkTreeViewColumn" -> "GTK_TREE_VIEW_COLUMN"
  | "GtkWidget" -> "GTK_WIDGET"
  | "GtkWindow" -> "GTK_WINDOW"
  | "GdkCursor" -> "GDK_CURSOR" (* XXXX just for gtk3 not to be done for gtk2 *)
  | "GCancellable" -> "G_CANCELLABLE"
  | _ -> object_data.od_cast;
;;

let object_arg_write_param object_data _oname params info _byref=
  let od_cast = cast_name object_data in
  if params.pnull then
    let ( varlist, code) =
    match params.pdflt with
    | None ->
	let varlist1 =varlist_add info.varlist object_data.od_objname ("*" ^ params.pname ^ " = NULL") in
	let varlist1 = varlist_add varlist1 "NspObject" ("*nsp_" ^ params.pname ^ " = NULL") in
	(varlist1, null params.pname object_data.od_objname od_cast)
    | Some x ->
	let varlist1 = varlist_add info.varlist object_data.od_objname  ("*" ^ params.pname ^ " = " ^ x) in
	let varlist1 = varlist_add varlist1 "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	(varlist1, nulldflt params.pname object_data.od_objname  od_cast) in
    let info = add_parselist info params.pvarargs "obj"  ["&nsp_" ^ params.pname]  [params.pname] in
    { info with
      arglist = params.pname :: info.arglist;
      varlist = varlist ;
      codebefore = code :: info.codebefore ;
    }
  else (* # remove nsp prefix in object_data.od_objname *)
    let tn = String.lowercase_ascii object_data.od_objname in
    let tn = if (String.sub tn 0 3) = "nsp" then String.sub tn 3 ((String.length tn) -3) else tn in
    let ( varlist, code, info, pname ) =
      match params.pdflt with
      | Some x ->
	  let varlist1 = varlist_add info.varlist object_data.od_objname ("*" ^ params.pname ^ " = " ^ x) in
	  let varlist1 = varlist_add varlist1 "NspObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	  ( varlist1, dflt params.pname od_cast ,
	    add_parselist info params.pvarargs "obj_check"
	      [("&nsp_type_" ^ tn); ("&nsp_" ^ params.pname) ]  [params.pname], params.pname)
      | None ->
	  let nsp_obj =
	    match object_data.od_cast with
	    | "GObject" -> "NspGObject"
	    | _ -> "NspObject" in
	  let varlist1 = varlist_add  info.varlist nsp_obj  ("*" ^ params.pname  ^ " = NULL") in
	  ( varlist1, "",
	    add_parselist info params.pvarargs "obj_check"  ["&nsp_type_" ^ tn; "&" ^ params.pname]  [params.pname],
	    match object_data.od_cast with
	    | "GObject" ->
		Printf.sprintf "G_OBJECT(%s->obj)" params.pname
	    | "GdkWindow" ->
		Printf.sprintf "GDK_WINDOW(%s->%s)" params.pname params.pname
	    | _ ->
		Printf.sprintf "((%s *) %s)" object_data.od_cast params.pname ) in
    { info with
      arglist = pname :: info.arglist;
      varlist = varlist ;
      codebefore = code :: info.codebefore ;
    }
;;

let object_arg_attr_write_set object_data oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist object_data.od_objname ("*" ^ params.pname)
    | Some x ->
	varlist_add  info.varlist object_data.od_objname ("*" ^ params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs object_data.od_nsp_arg_type ["&" ^ params.pname]  [params.pname] in
  let codebefore = (Printf.sprintf "  if ( ! Is%s(O) ) return FAIL;\n"  object_data.od_shortname )
    ^ (Printf.sprintf "  if ((%s = (%s *) nsp_object_copy_and_name(attr,O)) == NULL%s) return FAIL;\n"
	 params.pname object_data.od_objname object_data.od_shortname_uc)
    ^
      (
       if byref then
	 (Printf.sprintf	"  if (((%s *) self)->obj->%s != NULL ) \n"
	    oname params.pname)
	 ^ (Printf.sprintf "    nsp_%s_destroy(((%s *) self)->obj->%s);\n"
	      (String.lowercase_ascii object_data.od_name) oname params.pname)
       else
	 (Printf.sprintf  "  if (((%s *) _self)->%s != NULL ) \n" oname params.pname)
	 ^(Printf.sprintf"    nsp_%s_destroy(((%s *) self)->%s);\n"
	     (String.lowercase_ascii object_data.od_name) oname params.pname)
      )
    ^ ((* #pos gives the position of the argument *)
      if params.psize <> "" then
	(Printf.sprintf "/*  %s << size %s*/\n" params.pname  params.psize )
      else
	""
     ) in
  let attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    setobj = true;
    attrcodebefore = attrcodebefore :: info.attrcodebefore;
    codebefore = codebefore :: info.codebefore;
  }
;;

let object_arg_write_return _object_data ptype _ownsreturn info =
  let (_flag, ptype) = strip_type ptype in
  let varlist = varlist_add  info.varlist  ptype "*ret" in
  let codeafter =
    "  if (ret == NULL ) return RET_BUG;\n  MoveObj(stack,1,NSP_OBJECT(ret));\n  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let object_arg_attr_write_return  _object_data _objinfo _ownsreturn params info=
  let (_flag, ptype) = strip_type params.ptype in
  let varlist = varlist_add  info.varlist ptype "*ret" in
  let attrcodeafter = "  return NSP_OBJECT(ret);" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let object_arg_attr_equal_fields  _object_data  objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  (Printf.sprintf "if ( NSP_OBJECT(A->%s)->type->eq(A->%s,loc->%s) == FALSE ) return FALSE;\n"
     pname pname pname)
;;

let object_arg_attr_free_fields  _object_data _ptype pname _varname byref =
  let str = if byref then "  " else "" in
  str
  ^ (Printf.sprintf "  if (%s->%s != NULL)\n"  _varname pname )
  ^ (Printf.sprintf "    nsp_object_destroy((NspObject **)&%s->%s);\n"  _varname pname )
;;

let object_arg_attr_write_print _object_data _objinfo print_mode varname params =
  let arg = if print_mode = "latex" then "FALSE" else "indent+2" in
  Printf.sprintf
    "  if ( %s->%s != NULL)\n    { if ( nsp_object_%s(NSP_OBJECT(%s->%s),%s,\"%s\", rec_level+1)== FALSE ) return FALSE ;\n    }\n"
    varname  params.pname print_mode varname  params.pname arg params.pname
;;

let object_arg_attr_write_init _object_data _objinfo varname params =
    (Printf.sprintf "  %s->%s = NULL;\n" varname params.pname )
;;

let object_arg_attr_write_defval object_data _objinfo varname params =
  (Printf.sprintf "  if ( %s->%s == NULL) \n    {\n"  varname params.pname )
  ^ (Printf.sprintf
       "     if (( %s->%s = nsp_%s_create_default(\"%s\")) == NULL)\n       return FAIL;\n    }\n"
       varname params.pname (String.lowercase_ascii object_data.od_name) params.pname)
;;

let object_arg_attr_write_copy   object_data _objinfo params left_varname right_varname _f_copy_name =
  if right_varname <> "" then
    (Printf.sprintf "  if ( %s->%s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n"
       right_varname params.pname left_varname params.pname )
    ^ (Printf.sprintf
	 "      if ((%s->%s = (%s *) %s_and_name(\"%s\", NSP_OBJECT(%s->%s))) == NULL) return NULL;\n    }\n"
	 left_varname params.pname object_data.od_objname _f_copy_name params.pname right_varname params.pname )
  else
    (*
       (Printf.sprintf "  if ( %s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n" params.pname,left_varname,pname)
       ^(Printf.sprintf "      if ((%s->%s = (%s * )  zzz%s_and_name(\"%s\" NSP_OBJECT(%s))) == NULL) return NULL;\n    }\n"
       left_varname,pname,object_data.od_objname,_f_copy_name,pname,pname)
     *)
    (Printf.sprintf "  %s->%s= %s;\n" left_varname params.pname params.pname )
;;

let object_arg_attr_write_save _object_data _varname params _byref=
  (Printf.sprintf "  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(%s->%s)) == FAIL) return FAIL;\n"  _varname params.pname )
;;

let object_arg_attr_write_load object_data varname params _byref=
  Printf.sprintf "  if ((%s->%s= (%s *) nsp_object_xdr_load(xdrs))== NULL) return NULL;\n"
    varname params.pname object_data.od_objname
;;

let make_object_arg  object_data =
  { argtype with
    write_param = object_arg_write_param object_data;
    attr_write_set = object_arg_attr_write_set object_data;
    write_return = object_arg_write_return object_data;
    attr_write_return = object_arg_attr_write_return object_data;
    attr_free_fields = object_arg_attr_free_fields object_data;
    attr_write_save = object_arg_attr_write_save object_data;
    attr_write_load = object_arg_attr_write_load object_data;
    attr_write_copy = object_arg_attr_write_copy object_data;
    (* attr_write_info = object_arg_attr_write_info object_data; *)
    attr_write_print = object_arg_attr_write_print object_data;
    attr_write_init = object_arg_attr_write_init object_data;
    attr_equal_fields = object_arg_attr_equal_fields object_data;
    attr_write_defval = object_arg_attr_write_defval object_data;
 }
;;


(* gtk_object_arg: used when a NspGObject is used as attribute or argument.
 *------------------------------------------------------------------
 *)

let gtk_object_arg_write_param object_data _oname params info _byref=
  let od_cast = cast_name object_data in
  if params.pnull then
    let ( varlist, code) =
    match params.pdflt with
    | None ->
	let varlist1 =varlist_add info.varlist object_data.od_objname ("*" ^ params.pname ^ " = NULL") in
	let varlist1 = varlist_add varlist1 "NspGObject" ("*nsp_" ^ params.pname) in
	(varlist1, null params.pname object_data.od_objname od_cast)
    | Some x ->
	let varlist1 = varlist_add info.varlist object_data.od_objname  ("*" ^ params.pname ^ " = " ^ x) in
	let varlist1 = varlist_add varlist1 "NspGObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	(varlist1, nulldflt params.pname object_data.od_objname od_cast) in
    let info = add_parselist info params.pvarargs "obj"  ["&nsp_" ^ params.pname]  [params.pname] in
    { info with
      arglist = params.pname :: info.arglist;
      varlist = varlist ;
      codebefore = code :: info.codebefore ;
    }
  else (* # remove nsp prefix in object_data.od_objname *)
    let tn = String.lowercase_ascii object_data.od_objname in
    let tn = if (String.sub tn 0 3) = "nsp" then String.sub tn 3 ((String.length tn) -3) else tn in
    let ( varlist, code, info, pname ) =
      match params.pdflt with
      | Some x ->
	  let varlist1 = varlist_add info.varlist object_data.od_objname ("*" ^ params.pname ^ " = " ^ x) in
	  let varlist1 = varlist_add varlist1 "NspGObject"  ("*nsp_" ^ params.pname ^ " = NULL") in
	  ( varlist1, dflt params.pname od_cast ,
	    add_parselist info params.pvarargs "obj_check"
	      [("&nsp_type_" ^ tn); ("&nsp_" ^ params.pname) ]  [params.pname], params.pname)
      | None ->
	  let varlist1 = varlist_add  info.varlist "NspGObject"  ("*" ^ params.pname) in
	  ( varlist1, "",
	    add_parselist info params.pvarargs "obj_check"  ["&nsp_type_" ^ tn; "&" ^ params.pname]  [params.pname],
	    let cast =
	      Str.global_replace (Str.regexp "_TYPE_") "_" object_data.od_nsp_arg_type in
	    (Printf.sprintf "%s(%s->obj)" cast params.pname  )) in
    { info with
      arglist = pname :: info.arglist;
      varlist = varlist ;
      codebefore = code :: info.codebefore ;
    }
;;

let gtk_object_arg_write_return object_data ptype ownsreturn info =
 let (_flag, ptype) = strip_type ptype in
 let varlist = varlist_add  info.varlist  ptype "*ret" in
 let varlist = varlist_add  varlist "NspObject" "*nsp_ret" in
 let name = String.lowercase_ascii object_data.od_objname in
 let unref_code = if ownsreturn then "\n  g_object_unref(ret);" else "" in 									
 let codeafter =
   (* See genmethods.ml to see the difference between 
    *   nspgobject_new_with_possible_type 
    *   nspgobject_new
    *   gobject_create 
    * we prefer nspgobject_new_with_possible_type to return the most specialized type if available in nsp 
    *) 
    Printf.sprintf
	"  nsp_type_%s = new_type_%s(T_BASE);\
        \n  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_%s))== NULL) return RET_BUG;\
        %s\
        \n  MoveObj(stack,1,nsp_ret);\n  return 1;" name name name unref_code;
   in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let gtk_object_arg_attr_write_return object_data _objinfo ownsreturn params info=
  let (_flag, ptype) = strip_type params.ptype in
  let varlist = varlist_add  info.varlist ptype "*ret" in
  let varlist =
    if ownsreturn then
      varlist_add  varlist "NspObject" "*nsp_ret"
    else
      varlist in
  let attrcodeafter = 									
    if true then
     (
     if ownsreturn then
      (
       let name = String.lowercase_ascii object_data.od_objname in
       Printf.sprintf
	 "  nsp_type_%s = new_type_%s(T_BASE);\
	 \n  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret, (NspTypeBase *) nsp_type_%s))== NULL) return NULL;\
         \n  g_object_unref(ret);\
         \n  return nsp_ret;" name name name;
      )
    else
      (
       let name = String.lowercase_ascii object_data.od_objname in
       Printf.sprintf
	 "  nsp_type_%s = new_type_%s(T_BASE);\
	 \n  return (NspObject *) gobject_create(NVOID,(GObject *)ret, (NspTypeBase *) nsp_type_%s);"   name name name;
      )
    )
  else
     (
     if ownsreturn then
      (
       let name = String.lowercase_ascii object_data.od_objname in
       Printf.sprintf
	 "  nsp_type_%s = new_type_%s(T_BASE);\
	 \n  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *)ret))== NULL) return NULL;\
         \n  g_object_unref(ret);\
         \n  return nsp_ret;" name name ;
      )
    else
      (
       let name = String.lowercase_ascii object_data.od_objname in
       Printf.sprintf
	 "  nsp_type_%s = new_type_%s(T_BASE);\
	 \n  return (NspObject *) nspgobject_new(NVOID,(GObject *)ret))== NULL) return NULL;" name name;
      )
    )
  in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let make_gtk_object_arg  object_data =
  { argtype with
    write_param = gtk_object_arg_write_param object_data;
    attr_write_set = object_arg_attr_write_set object_data;
    write_return = gtk_object_arg_write_return object_data;
    attr_write_return = gtk_object_arg_attr_write_return object_data;
    attr_free_fields = object_arg_attr_free_fields object_data;
    attr_write_save = object_arg_attr_write_save object_data;
    attr_write_load = object_arg_attr_write_load object_data;
    attr_write_copy = object_arg_attr_write_copy object_data;
    (* attr_write_info = object_arg_attr_write_info object_data; *)
    attr_write_print = object_arg_attr_write_print object_data;
    attr_write_init = object_arg_attr_write_init object_data;
    attr_equal_fields = object_arg_attr_equal_fields object_data;
    attr_write_defval = object_arg_attr_write_defval object_data;
 }
;;

(* nsp_object_arg *)

let nsp_object_arg_write_param nsp_generic_data oname params info byref=
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist nsp_generic_data.ng_fullname ("*" ^ params.pname ^ " = NULL")
    | Some x ->
      varlist_add info.varlist nsp_generic_data.ng_fullname ("*" ^ params.pname ^ " = " ^ x) in
  let info = add_parselist info params.pvarargs "obj"  ["&" ^ params.pname]  [params.pname] in
  let attrcodebefore =
    (Printf.sprintf
       "TTT  if ((%s = (%s *) nsp_object_copy_and_name(attr,O)) == NULL%s) return FAIL;\n"
       params.pname nsp_generic_data.ng_fullname nsp_generic_data.ng_shortname_uc)
    ^ (
      if byref then
	(
	 (Printf.sprintf "  if (((%s *) self)->obj->%s != NULL ) \n" oname params.pname)
	 ^ (Printf.sprintf "    nsp_%s_destroy(&((%s *) self)->obj->%s);\n"
	      (String.lowercase_ascii nsp_generic_data.ng_name) oname params.pname)
	)
      else
	(
	 (Printf.sprintf "  if (((%s *) self)->%s != NULL ) \n" oname params.pname)
	 ^  (Printf.sprintf
	       "    nsp_%s_destroy(&((%s *) self)->%s);\n"
	     (String.lowercase_ascii nsp_generic_data.ng_name) oname params.pname)
	)
     )
    ^ (
      if  params.psize <> "" then
	(Printf.sprintf "/*  %s << size %s*/\n"  params.pname  params.psize)
      else "") in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist ;
    attrcodebefore = attrcodebefore :: info.attrcodebefore ;
    setobj = true;
  }
;;

let nsp_object_arg_attr_write_set nsp_generic_data oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = nsp_object_arg_write_param nsp_generic_data  oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  %s= %s;\n" pset_name params.pname) :: info.attrcodebefore;}
;;

let nsp_object_arg_write_return _nsp_generic_data _ptype _ownsreturn info =
  let varlist = varlist_add  info.varlist  "NspObject" "*ret" in
  let  codeafter =
    "  if (ret == NULLOBJ ) return RET_BUG;\n\
      MoveObj(stack,1,ret);\n  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let nsp_object_arg_attr_write_return _nsp_generic_data _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist "NspObject" "*ret" in
  let attrcodeafter = " return ret;" in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let nsp_object_arg_attr_write_print _nsp_generic_data _objinfo print_mode varname params =
  if print_mode = "latex" then
    Printf.sprintf
      "        if ( %s->%s->type->latex(%s->%s,FALSE,\"%s\",rec_level+1)==FALSE) return FALSE;\n"
      varname params.pname varname params.pname params.pname
  else
    Printf.sprintf
      "        if ( %s->%s->type->pr(%s->%s,indent+2,\"%s\",rec_level+1)==FALSE) return FALSE;\n"
      varname params.pname varname params.pname params.pname
;;

let nsp_object_arg_attr_write_init nsp_generic_data _objinfo varname params =
    Printf.sprintf "  %s->%s = NULL%s;\n" varname params.pname nsp_generic_data.ng_shortname_uc
;;

let nsp_object_arg_attr_equal_fields _nsp_generic_data objinfo _varname params=
  let pname = if objinfo.or_byref then "obj->"^ params.pname else params.pname in
  Printf.sprintf "  if ( NSP_OBJECT(A->%s)->type->eq(A->%s,loc->%s) == FALSE ) return FALSE;\n"
    pname pname pname
;;

let nsp_object_arg_attr_write_save varname params _byref=
  Printf.sprintf "  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(%s->%s)) == FAIL) return FAIL;\n"
    varname params.pname
;;

let nsp_object_arg_attr_write_load nsp_generic_data varname params _byref=
    Printf.sprintf "  if ((%s->%s =(%s *) nsp_object_xdr_load(xdrs))== NULL%s) return NULL;\n"
    varname params.pname nsp_generic_data.ng_fullname nsp_generic_data.ng_shortname_uc
;;

let nsp_object_arg_attr_free_fields _nsp_generic_data _ptype pname _varname byref =
  let str = if byref then "  " else "" in
  str
  ^ (Printf.sprintf "  if (%s->%s != NULL)\n"  _varname pname )
  ^ (Printf.sprintf "    nsp_object_destroy(&%s->%s);\n"  _varname pname )
;;

let nsp_object_arg_attr_write_copy  nsp_generic_data _objinfo params left_varname right_varname f_copy_name =
  if right_varname <> "" then
    (Printf.sprintf
       "  if ( %s->%s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n"
     right_varname params.pname left_varname params.pname)
    ^ (Printf.sprintf
	 "      if ((%s->%s = (%s *) %s_and_name(\"%s\",NSP_OBJECT(%s->%s))) == NULL%s) return NULL;\n    }\n"
         left_varname params.pname nsp_generic_data.ng_fullname f_copy_name params.pname right_varname params.pname nsp_generic_data.ng_shortname_uc)
  else
    (Printf.sprintf  "  %s->%s= %s;\n" left_varname params.pname params.pname)
;;

let nsp_object_arg_attr_write_defval nsp_generic_data _objinfo varname params =
  (Printf.sprintf
     "  if ( %s->%s == NULL%s) \n    {\n"
     varname params.pname nsp_generic_data.ng_shortname_uc)
  ^ (Printf.sprintf
       "     if (( %s->%s =(NspObject*) nsp_matrix_create(\"%s\",'r',0,0)) == NULL)\
       \n       return FAIL;\
       \n    }\n"
       varname params.pname params.pname
     )
;;

let make_nsp_object_arg  ngd =
  { argtype with
    write_param = nsp_object_arg_write_param ngd;
    attr_write_set = nsp_object_arg_attr_write_set ngd;
    write_return = nsp_object_arg_write_return ngd;
    attr_write_return = nsp_object_arg_attr_write_return ngd;
    attr_free_fields = nsp_object_arg_attr_free_fields ngd;
    attr_write_save = nsp_object_arg_attr_write_save;
    attr_write_load = nsp_object_arg_attr_write_load ngd;
    attr_write_copy = nsp_object_arg_attr_write_copy ngd;
    (* attr_write_info = nsp_object_arg_attr_write_info ngd; *)
    attr_write_print = nsp_object_arg_attr_write_print ngd;
    attr_write_init = nsp_object_arg_attr_write_init ngd;
    attr_equal_fields = nsp_object_arg_attr_equal_fields ngd;
    attr_write_defval = nsp_object_arg_attr_write_defval ngd;
 }
;;

(* struct_arg : declared with define-struct or define-structref
 * we assume that a Nsp class is associated and has the same name
 * when a struct arg is used as parameter or returned we make a copy
 * ----------------------------------------------------------------
 *)

let struct_check object_rec name =
  let typename = object_rec.or_c_name in
  let byref = object_rec.or_byref in
  let tag = if byref then "obj->" else "" in
  Printf.sprintf "  if ( Is%s(nsp_%s))\
  \n    { %s = ((Nsp%s *) nsp_%s)->%svalue;\
  \n      if((%s = nsp_copy_%s(%s))==NULL) return RET_BUG;\
  \n    }\
  \n  else\
  \n    {\
  \n      Scierror(\"Error: %s should be of type %s\\n\");\
  \n      return RET_BUG;\
  \n    }\n" typename name name typename name tag name typename name name typename
;;

let struct_null object_rec name =
  let typename = object_rec.or_c_name in
  let byref = object_rec.or_byref in
  let tag = if byref then "obj->" else "" in
  Printf.sprintf
    "  if (nsp_%s != NULL)\
  \n     {\
  \n      if( Is%s(nsp_%s))\
  \n        { %s = ((Nsp%s *) nsp_%s)->%svalue;}\
  \n      else\
  \n        {Scierror(\"Error: %s should be of type %s\\n\");\
  \n         return RET_BUG;\
  \n        }\
  \n      if((%s = nsp_copy_%s(%s))==NULL) return RET_BUG;\
  \n     }\n" name typename name name typename name tag name typename name typename name
  ;;

(* no need to make a copy if required argument is const *)

let struct_check_const object_rec name =
  let typename = object_rec.or_c_name in
  let byref = object_rec.or_byref in
  let tag = if byref then "obj->" else "" in
  Printf.sprintf "  if ( Is%s(nsp_%s))\
  \n    { %s = ((Nsp%s *) nsp_%s)->%svalue;\
  \n    }\
  \n  else\
  \n    {\
  \n      Scierror(\"Error: %s should be of type %s\\n\");\
  \n      return RET_BUG;\
  \n    }\n" typename name name typename name tag name typename
;;

(* no need to make a copy if required argument is const *)

let struct_null_const object_rec name =
  let typename = object_rec.or_c_name in
  let byref = object_rec.or_byref in
  let tag = if byref then "obj->" else "" in
  Printf.sprintf
    "  if (nsp_%s != NULL)\
  \n     {\
  \n      if( Is%s(nsp_%s))\
  \n        { %s = ((Nsp%s *) nsp_%s)->%svalue;}\
  \n      else\
  \n        {Scierror(\"Error: %s should be of type %s\\n\");\
  \n         return RET_BUG;\
  \n        }\
  \n     }\n" name typename name name typename name tag name typename 
  ;;

(* when a struct_arg is a parameter *)

let string_is_const str = 
  let start = String.sub str 0 6 in
  start = "const-"
;;

let struct_arg_write_param_gen object_rec _oname params info _byref set_value =
  let is_const = string_is_const params.ptype in
  let name = params.pname in
  let ctype = 
    if is_const then  ("const " ^ object_rec.or_c_name) 
    else object_rec.or_c_name  in
  let varlist = varlist_add info.varlist ctype ("*" ^ name ^ " = NULL") in
  let codebefore =
    if params.pnull then
      if is_const then
	struct_null_const object_rec name
      else
	struct_null object_rec name
    else
      if is_const then
	struct_check_const object_rec name 
      else
	struct_check object_rec name 
  in
  let initial_value =
    if set_value = "" then " = NULL"  else set_value in
  let varlist = varlist_add varlist "NspObject" ("*nsp_" ^ name ^  initial_value) in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_" ^ name] [name] in
  { info with
    arglist = name :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
    setobj = true;
  }
;;

let struct_arg_write_param object_rec _oname params info _byref =
  struct_arg_write_param_gen object_rec _oname params info _byref ""
;;

(* when the field is set *)

let struct_arg_attr_write_set object_rec oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = struct_arg_write_param_gen object_rec oname params info byref "= O" in
  { info with attrcodebefore = (Printf.sprintf "  /* %s= %s;*/\n" pset_name params.pname) :: info.attrcodebefore;}
;;

(* when it's a returned value *)

let struct_arg_write_return object_rec ptype _ownsreturn info =
  let (flag, new_type) = strip_type ptype in
  let is_const =  string_is_const new_type in
  let tag = if flag then "" else "&" in
  let tagdec = if flag then "*" else "" in
  let declaration = 
    if is_const then
      ("const " ^ object_rec.or_c_name) 
    else
      object_rec.or_c_name in
  let varlist = varlist_add  info.varlist declaration (tagdec ^ "ret") in
  let varlist = 
    if is_const then
      varlist_add  varlist object_rec.or_c_name (tagdec ^ "ret1") 
    else
      varlist in
  let name = object_rec.or_name in
  let varlist = varlist_add varlist "NspObject" ("*nsp_ret") in
  let codeafter =
    if is_const then
      "  nsp_type_" ^ name ^"= new_type_"^ name^"(T_BASE);\n" ^
      "  if((ret1 = nsp_copy_"^ object_rec.or_c_name ^"(ret))==NULL) return RET_BUG;\n" ^
      "  nsp_ret =(NspObject*) nsp_"^ name^"_create(NVOID,"^
      tag ^ "ret1,(NspTypeBase *) nsp_type_"^ name ^");\n" ^
      "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
      "  MoveObj(stack,1,nsp_ret);\n  return 1;"
    else
      "  nsp_type_" ^ name ^"= new_type_"^ name^"(T_BASE);\n" ^
      "  if((ret = nsp_copy_"^ object_rec.or_c_name ^"(ret))==NULL) return RET_BUG;\n" ^
      "  nsp_ret =(NspObject*) nsp_"^ name^"_create(NVOID,"^
      tag ^ "ret,(NspTypeBase *) nsp_type_"^ name ^");\n" ^
      "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
      "  MoveObj(stack,1,nsp_ret);\n  return 1;"
  in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

(* when an attribute is returned  *)

let struct_arg_attr_write_return object_rec _objinfo _ownsreturn params info=
  let (flag, _ptype) = strip_type params.ptype in
  let tag = if flag then "" else "&" in
  let tagdec = if flag then "*" else "" in
  let varlist = varlist_add  info.varlist object_rec.or_c_name (tagdec ^ "ret") in
  let name = object_rec.or_name in
  let varlist = varlist_add varlist "NspObject" ("*nsp_ret") in
  let codeafter =
    "  nsp_type_" ^ name ^"= new_type_"^ name^"(T_BASE);\n" ^
    "  if((ret = nsp_copy_"^ object_rec.or_c_name ^"(ret))==NULL) return NULL;\n" ^
    "  nsp_ret =(NspObject*) nsp_"^ name^"_create(NVOID,"^
    tag ^ "ret,(NspTypeBase *) nsp_type_"^ name ^");\n" ^
    "  return nsp_ret;" in
  { info with varlist = varlist ; attrcodeafter = codeafter :: info.attrcodeafter ;}
;;

let struct_arg_attr_free_fields _object_rec ptype pname _varname _byref =
  let name = if _byref then  "obj->" ^ pname else pname in
  let (flag, ptype) = strip_type ptype in
  if flag then
    (Printf.sprintf "  if (H->%s != NULL)\n"  name)
    ^ (Printf.sprintf "    { nsp_destroy_%s(H->%s,H);}\n" ptype name )
  else
   Printf.sprintf "  nsp_destroy_%s(&H->%s,H); \n"  ptype name
;;

let struct_arg_attr_write_save _varname params _byref=
  let pname = if _byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf "  if ( nsp_save_%s(xdrs,M->%s,M) == FAIL ) return FAIL;\n" ptype pname
  else
    Printf.sprintf "  if ( nsp_save_%s(xdrs,&M->%s,M) == FAIL ) return FAIL;\n" ptype pname
;;

let struct_arg_attr_write_load _object_rec _varname params _byref=
  let pname = if _byref then  "obj->" ^ params.pname else params.pname in
  let (_flag, ptype) = strip_type params.ptype in
  (Printf.sprintf "  if ( nsp_load_%s(xdrs,M->%s,M) == FAIL ) return NULL;\n" ptype pname)
;;

let rstrip str str_strip =
  Str.global_replace (Str.regexp str_strip) "" str
;;

let struct_arg_attr_write_copy  _object_rec _objinfo params left_varname right_varname f_copy_name =
  let name = params.pname in
  if right_varname <> "" then
    if f_copy_name = "nsp_object_full_copy" then
      let (flag, ptype) = strip_type params.ptype in
      let tag = if flag then "" else "&" in
      let vn = rstrip right_varname "->obj" in
      let vl = rstrip left_varname "->obj" in
      Printf.sprintf "  if( nsp_%s_full_copy(%s,%s%s->%s,%s)== FAIL) return NULL;\n"
        ptype vl tag left_varname name vn
    else
      Printf.sprintf "  %s->%s = %s->%s;\n" left_varname name right_varname name
  else
    Printf.sprintf "  %s->%s = %s;\n" left_varname name name
;;

let struct_arg_attr_write_info _ptype pname varname _byref =
  (Printf.sprintf "ZZ  Sciprintf1(indent+2,\"%s=0x%%x\\n\" %s->%s);\n"  pname varname pname)
;;

let struct_arg_attr_write_print _object_rec objinfo _print_mode varname params =
  (*  varname here already contains ->obj *)
  let vn = rstrip varname "->obj" in
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf  "  nsp_print_%s(indent+2,%s->%s,%s);\n" ptype vn pname vn
  else
    Printf.sprintf  "  nsp_print_%s(indent+2,&%s->%s,%s);\n" ptype vn pname vn
;;

let struct_arg_attr_write_init _object_rec _objinfo varname params =
  let (flag, ptype) = strip_type params.ptype in
  let pname = params.pname in
  if flag then
    Printf.sprintf "  %s->%s = NULL;\n" varname pname
  else
    Printf.sprintf"  nsp_init_%s(&%s->%s);\n" ptype  varname  pname
;;

let struct_arg_attr_equal_fields _object_rec objinfo _varname params=
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname
  else
    Printf.sprintf "  if ( nsp_eq_%s(&A->%s,&loc->%s)== FALSE) return FALSE;\n"  ptype pname pname
;;

let struct_arg_attr_write_defval _object_rec objinfo _varname params =
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf "  if ( nsp_check_%s(H->%s,H) == FAIL ) return FAIL;\n"  ptype pname
  else
    Printf.sprintf "  if ( nsp_check_%s(&H->%s,H) == FAIL ) return FAIL;\n" ptype pname
;;

let make_struct_arg object_rec =
  { argtype with
    write_param = struct_arg_write_param object_rec;
    attr_write_set = struct_arg_attr_write_set object_rec;
    write_return = struct_arg_write_return object_rec;
    attr_write_return = struct_arg_attr_write_return  object_rec;
    attr_free_fields = struct_arg_attr_free_fields  object_rec;
    attr_write_save = struct_arg_attr_write_save;
    attr_write_load = struct_arg_attr_write_load  object_rec;
    attr_write_copy = struct_arg_attr_write_copy  object_rec;
    attr_write_info = struct_arg_attr_write_info;
    attr_write_print = struct_arg_attr_write_print  object_rec;
    attr_write_init = struct_arg_attr_write_init  object_rec;
    attr_equal_fields = struct_arg_attr_equal_fields  object_rec;
    attr_write_defval = struct_arg_attr_write_defval  object_rec;
  }
;;

(* boxed_arg *)

type boxed_data = {
    bd_typename: string;
    bd_typecode: string;
  }
;;

let boxed_check name typename typecode =
  Printf.sprintf "  if (nspg_boxed_check(nsp_%s, %s))\
   \n      %s = nspg_boxed_get(nsp_%s, %s);\
   \n  else {\
   \n      Scierror( \"Error: %s should be a %s\\n\");\
   \n      return RET_BUG;\
   \n  }\n" name typecode name name typename name typename
;;

let boxed_null name typename typecode =
  Printf.sprintf "  if ( nsp_%s != NULL ) {\
    \n    if (nspg_boxed_check(nsp_%s, %s))\
    \n      %s = nspg_boxed_get(nsp_%s, %s);\
    \n    else if (! IsNone(nsp_%s)) {\
    \n      Scierror(\"Error: %s should be a %s or None\\n\");\
    \n      return RET_BUG;\
    \n    }\
    \n  }\n" name name typecode name name typename name name typename
;;

let boxed_arg_write_param boxed_data _oname params info _byref=
  let strip_const str =
    Str.global_replace (Str.regexp "const-") "" str in
  let pname = params.pname in
  let (varlist, codebefore ) =
    if params.pnull then
      let varlist = varlist_add info.varlist boxed_data.bd_typename ( "*"  ^  pname  ^  " = NULL") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL"),
	boxed_null pname boxed_data.bd_typename boxed_data.bd_typecode)
    else
      let varlist = varlist_add info.varlist boxed_data.bd_typename ( "*"  ^  pname  ^  " = NULL") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname ^ " = NULL"),
	boxed_check pname boxed_data.bd_typename boxed_data.bd_typecode) in
  let (flag, new_type) = strip_type params.ptype in
  let arglist =
    if flag then
      let typename = new_type in
      let typename = strip_const typename in
      if typename <> boxed_data.bd_typename then
	Printf.sprintf "(%s *)%s" new_type pname
      else
	pname
    else
      pname in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname] in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let boxed_arg_write_return boxed_data ptype ownsreturn info =

  let (flag, _new_type) = strip_type ptype in
   let (varlist, ret, ownsreturn ) =
    if flag then
       ( varlist_add info.varlist boxed_data.bd_typename ( "*ret") ,  "ret", ownsreturn)
    else
       ( varlist_add info.varlist boxed_data.bd_typename ( "ret"),  "&ret", false) in

  let varlist = varlist_add varlist "NspObject" ("*nsp_ret"); in

  let tag =  if ownsreturn then "FALSE" else "TRUE" in
  let codeafter =
    Printf.sprintf
      "  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,%s, %s, %s, TRUE,\
     \n                                             (NspTypeBase *) %s))== NULL)\
     \n    return RET_BUG;\
     \n  MoveObj(stack,1,nsp_ret);\
     \n  return 1;"
     boxed_data.bd_typecode ret tag ("nsp_type_" ^ (String.lowercase_ascii boxed_data.bd_typename)) in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let boxed_arg_attr_write_return boxed_data _objinfo ownsreturn params info=
  let (flag, _new_type) = strip_type params.ptype in
  let varlist , ret, ownsreturn =
    if flag then
      (varlist_add info.varlist boxed_data.bd_typename ( "*ret"), "ret", ownsreturn )
    else
      (varlist_add info.varlist boxed_data.bd_typename ( "ret"), "&ret", false) in
  let tag =  if ownsreturn then "FALSE" else "TRUE" in
  let attrcodeafter =
  Printf.sprintf
      "  /* nspg_boxed_new handles NULL checking */\
     \n  return (NspObject *) gboxed_create(NVOID,%s, %s, %s, TRUE,(NspTypeBase *) %s);"
  boxed_data.bd_typecode ret tag
  ("nsp_type_" ^  (String.lowercase_ascii boxed_data.bd_typename)) in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let make_boxed_arg boxed_data =
  { argtype with
    write_param = boxed_arg_write_param boxed_data;
    (* attr_write_set = boxed_arg_attr_write_set boxed_data; *)
    write_return = boxed_arg_write_return boxed_data;
    attr_write_return = boxed_arg_attr_write_return  boxed_data;
    (* attr_free_fields = boxed_arg_attr_free_fields  boxed_data;
    attr_write_save = boxed_arg_attr_write_save;
    attr_write_load = boxed_arg_attr_write_load  boxed_data;
    attr_write_copy = boxed_arg_attr_write_copy  boxed_data;
    attr_write_info = boxed_arg_attr_write_info;
    attr_write_print = boxed_arg_attr_write_print  boxed_data;
    attr_write_init = boxed_arg_attr_write_init  boxed_data;
    attr_equal_fields = boxed_arg_attr_equal_fields  boxed_data;
    attr_write_defval = boxed_arg_attr_write_defval  boxed_data; *)
  }
;;


(* string_array_arg : char** NULL terminated array of string *)

let string_array_check cast name =
  Printf.sprintf "  if ( IsSMat(nsp_%s))\
  \n    { %s = %s ((NspSMatrix *) nsp_%s)->S;}\
  \n  else\
  \n    {\
  \n      Scierror(\"Error: %s should be of type SMat\\n\");\
  \n      return RET_BUG;\
  \n    }\n" name name cast name name
;;

let string_array_null cast name =
  Printf.sprintf "  if ( IsSMat(nsp_%s))\
  \n    %s = %s ((NspSMatrix *) nsp_%s)->S;\
  \n  else if ( ! IsNone(nsp_%s) ) {\
  \n      Scierror(\"Error: %s should be of type SMat\\n\");\
  \n      return RET_BUG;\
  \n  }\n" name name cast name name name
  ;;

let string_array_arg_write_param cast _oname params info _byref=
  let name = params.pname in
  let varlist = varlist_add info.varlist (cast ^ "gchar") ("**" ^ name ^ " = NULL") in
  let cast = if cast = "" then "" else "(" ^ cast ^ "gchar **)" in
  let codebefore =
    if params.pnull then
      string_array_null cast name
    else
      string_array_check cast name in
  let varlist =
    varlist_add varlist "NspObject" ("*nsp_" ^ name ^ " = NULL") in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_" ^ name] [name] in
  { info with
    arglist = name :: info.arglist;
    varlist = varlist ;
    codebefore = codebefore :: info.codebefore ;
    setobj = true;
  }
;;

let string_array_arg_attr_write_set cast oname params info byref=
  let pset_name = pset_name_set byref oname params.pname in
  let info = string_array_arg_write_param cast oname params info byref in
  { info with attrcodebefore = (Printf.sprintf "  /* %s= %s;*/\n" pset_name params.pname) :: info.attrcodebefore;}
;;

(* returning a gchar**: we assume that we can free the gchar** 
 * except if ownsreturn is not true.
 *)

let string_array_arg_write_return _ptype ownsreturn info =
  let varlist = 
    if ownsreturn then
       varlist_add  info.varlist  "const gchar * const *" "ret" 
    else
       varlist_add  info.varlist  "gchar" "**ret" 
  in
  let varlist = varlist_add varlist "NspObject" "*nsp_ret" in
  let codeafter =
    if ownsreturn then
      "  nsp_ret = (NspObject *) nsp_smatrix_create_from_const_table(ret);\n" ^
      "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
      "  MoveObj(stack,1,nsp_ret);\n  return 1;" 
    else
      "  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);\n" ^
      "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
      "  g_strfreev(ret);\n" ^
      "  MoveObj(stack,1,nsp_ret);\n  return 1;" 
  in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let string_array_arg_attr_write_return cast _objinfo _ownsreturn _params info=
  let varlist = varlist_add  info.varlist (cast ^ "gchar") "**ret" in
  let code = "  return NULL;\n" in
  { info with varlist = varlist ; attrcodeafter = code :: info.attrcodeafter ;}
;;

let string_array_arg_attr_free_fields _ptype pname _varname _byref =
  let name = if _byref then  "obj->" ^ pname else pname in
  Printf.sprintf "  if (H->%s != NULL) { g_strfreev(H->%s);}\n" name name
;;

let string_array_arg_attr_write_save _varname params _byref=
  let pname = if _byref then  "obj->" ^ params.pname else params.pname in
  Printf.sprintf "  if ( nsp_save_string_array(xdrs,M->%s,M) == FAIL ) return FAIL;\n" pname
;;

let string_array_arg_attr_write_load _varname params _byref=
  let lname = "n_" ^ params.pname in
  let lname = if _byref then  "obj->" ^ lname else lname in
  let pname = if _byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    (Printf.sprintf "  if (( M->%s = malloc(M->%s*sizeof(%s))) == NULL )\n " pname lname ptype)
    ^ (Printf.sprintf "    return NULL;\n")
    ^ (Printf.sprintf "  if ( nsp_load_%s(xdrs,M->%s,M) == FAIL ) return NULL;\n" ptype pname)
  else
    Printf.sprintf "  if ( nsp_load_%s(xdrs,&M->%s,M) == FAIL ) return NULL;\n" ptype pname
;;

let rstrip str str_strip =
  Str.global_replace (Str.regexp str_strip) "" str
;;

let string_array_arg_attr_write_copy _objinfo params left_varname right_varname f_copy_name =
  let name = params.pname in
  if right_varname <> "" then
    if f_copy_name = "nsp_object_full_copy" then
      let (flag, ptype) = strip_type params.ptype in
      let tag = if flag then "" else "&" in
      let vn = rstrip right_varname "->obj" in
      let vl = rstrip left_varname "->obj" in
      Printf.sprintf "  if( nsp_%s_full_copy(%s,%s%s->%s,%s)== FAIL) return NULL;\n"
        ptype vl tag left_varname name vn
    else
      Printf.sprintf "  %s->%s = %s->%s;\n" left_varname name right_varname name
  else
    Printf.sprintf "  %s->%s = %s;\n" left_varname name name
;;

let string_array_arg_attr_write_info _ptype pname varname _byref =
  (Printf.sprintf "ZZ  Sciprintf1(indent+2,\"%s=0x%%x\\n\" %s->%s);\n"  pname varname pname)
;;

let string_array_arg_attr_write_print objinfo _print_mode varname params =
  (*  varname here already contains ->obj *)
  let vn = rstrip varname "->obj" in
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf  "  nsp_print_%s(indent+2,%s->%s,%s);\n" ptype vn pname vn
  else
    Printf.sprintf  "  nsp_print_%s(indent+2,&%s->%s,%s);\n" ptype vn pname vn
;;

let string_array_arg_attr_write_init  _objinfo varname params =
  let (flag, ptype) = strip_type params.ptype in
  let pname = params.pname in
  if flag then
    Printf.sprintf "  %s->%s = NULL;\n" varname pname
  else
    Printf.sprintf"  nsp_init_%s(&%s->%s);\n" ptype  varname  pname
;;

let string_array_arg_attr_equal_fields objinfo _varname params=
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf "  if ( A->%s != loc->%s) return FALSE;\n"  pname pname
  else
    Printf.sprintf "  if ( nsp_eq_%s(&A->%s,&loc->%s)== FALSE) return FALSE;\n"  ptype pname pname
;;

let string_array_arg_attr_write_defval objinfo _varname params =
  let pname = if objinfo.or_byref then  "obj->" ^ params.pname else params.pname in
  let (flag, ptype) = strip_type params.ptype in
  if flag then
    Printf.sprintf "  if ( nsp_check_%s(H->%s,H) == FAIL ) return FAIL;\n"  ptype pname
  else
    Printf.sprintf "  if ( nsp_check_%s(&H->%s,H) == FAIL ) return FAIL;\n" ptype pname
;;

let string_array_arg =
  { argtype with
    write_param = string_array_arg_write_param "" ;
    attr_write_set = string_array_arg_attr_write_set "" ;
    write_return = string_array_arg_write_return  ;
    attr_write_return = string_array_arg_attr_write_return  "";
    attr_free_fields = string_array_arg_attr_free_fields  ;
    attr_write_save = string_array_arg_attr_write_save;
    attr_write_load = string_array_arg_attr_write_load  ;
    attr_write_copy = string_array_arg_attr_write_copy  ;
    attr_write_info = string_array_arg_attr_write_info;
    attr_write_print = string_array_arg_attr_write_print  ;
    attr_write_init = string_array_arg_attr_write_init  ;
    attr_equal_fields = string_array_arg_attr_equal_fields  ;
    attr_write_defval = string_array_arg_attr_write_defval  ;
  }
;;

let const_string_array_arg =
  { argtype with
    write_param = string_array_arg_write_param "const ";
    attr_write_set = string_array_arg_attr_write_set "const " ;
    write_return = string_array_arg_write_return ;
    attr_write_return = string_array_arg_attr_write_return "const " ;
    attr_free_fields = string_array_arg_attr_free_fields  ;
    attr_write_save = string_array_arg_attr_write_save;
    attr_write_load = string_array_arg_attr_write_load  ;
    attr_write_copy = string_array_arg_attr_write_copy  ;
    attr_write_info = string_array_arg_attr_write_info;
    attr_write_print = string_array_arg_attr_write_print  ;
    attr_write_init = string_array_arg_attr_write_init  ;
    attr_equal_fields = string_array_arg_attr_equal_fields  ;
    attr_write_defval = string_array_arg_attr_write_defval  ;
  }
;;

(* custom_boxed_arg *)

type custom_boxed_data = {
    cbd_getter: string;
    cbd_checker: string;
    cbd_new: string;
    cbd_nsp_type: string;
  }
;;

let init_custom_boxed_data getter checker new_s type_s =
  {
   cbd_getter= getter;
   cbd_checker= checker;
   cbd_new= new_s;
   cbd_nsp_type= type_s;
 }
;;

let cba_null name get check ptype  =
  Printf.sprintf
    "  if (%s(nsp_%s))\
   \n      %s = %s(nsp_%s);\
   \n  else if ( ! IsNone(nsp_%s)) {\
   \n      Scierror( \"Error: %s should be a %s or None\\n\");\
   \n      return RET_BUG;\
   \n  }\n" check name name get name name name ptype
;;

let custom_boxed_arg_write_param custom_boxed_data _oname params info _byref=
  let pname = params.pname in
  let (_flag, new_type) = strip_type params.ptype in
  let (varlist, info, arglist,codebefore ) =
    if params.pnull then
      let varlist = varlist_add info.varlist new_type ( "*"  ^  pname  ^  " = NULL") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL"),
	add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname],
	pname,
	cba_null pname custom_boxed_data.cbd_getter custom_boxed_data.cbd_checker new_type)
    else
      ( varlist_add info.varlist "NspObject" ( "*"  ^  pname ^  " = NULL"),
	add_parselist info params.pvarargs "obj_check" ["&"  ^  custom_boxed_data.cbd_nsp_type ^ "&"  ^  pname] [pname],
	custom_boxed_data.cbd_getter  ^  "("  ^  pname  ^  ")",
	""
       ) in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
   }
;;

let custom_boxed_arg_write_return custom_boxed_data ptype _ownsreturn info =
  let (_flag, new_type) = strip_type ptype in
  let varlist = varlist_add info.varlist new_type ( "*ret") in
  let codeafter ="  if (ret == NULL) return RET_BUG;\n"  ^
    "  return "  ^  custom_boxed_data.cbd_new  ^  "(ret);" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let custom_boxed_arg_attr_write_return custom_boxed_data _objinfo _ownsreturn params info=
  let (_flag, new_type) = strip_type params.ptype in
  let varlist = varlist_add info.varlist new_type new_type in
  let attrcodeafter ="  return (ret == NULL) ? NULL : "  ^  custom_boxed_data.cbd_new  ^  "(ret);" in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let make_custom_boxed_arg custom_boxed_data =
  { argtype with
    write_param = custom_boxed_arg_write_param custom_boxed_data;
    (* attr_write_set = custom_boxed_arg_attr_write_set boxed_data; *)
    write_return = custom_boxed_arg_write_return custom_boxed_data;
    attr_write_return = custom_boxed_arg_attr_write_return  custom_boxed_data;
    (* attr_free_fields = custom_boxed_arg_attr_free_fields  boxed_data;
    attr_write_save = custom_boxed_arg_attr_write_save;
    attr_write_load = custom_boxed_arg_attr_write_load  boxed_data;
    attr_write_copy = custom_boxed_arg_attr_write_copy  boxed_data;
    attr_write_info = custom_boxed_arg_attr_write_info;
    attr_write_print = custom_boxed_arg_attr_write_print  boxed_data;
    attr_write_init = custom_boxed_arg_attr_write_init  boxed_data;
    attr_equal_fields = custom_boxed_arg_attr_equal_fields  boxed_data;
    attr_write_defval = custom_boxed_arg_attr_write_defval  boxed_data; *)
  }
;;


(* pointer_arg *)

type pointer_data = {
    pd_typename: string;
    pd_typecode: string;
  }
;;

let pa_check name typename typecode =
  Printf.sprintf "  if (nspg_pointer_check(nsp_%s, %s))\
   \n      %s = nspg_pointer_get(nsp_%s, %s);\
   \n  else {\
   \n      Scierror( \"Error: %s should be a %s\\n\");\
   \n      return RET_BUG;\
   \n  }\n"  name typecode name name typename name typename
;;

let pa_null name typename typecode =
  Printf.sprintf "  if (nspg_pointer_check(nsp_%s, %s))\
   \n      %s = nspg_pointer_get(nsp_%s, %s);\
   \n  else if ( ! IsNone(nsp_%s) ) {\
   \n      Scierror( \"Error: %s should be a %s or None\\n\");\
   \n      return RET_BUG;\
   \n  }\n" name typecode name name typename name name typename
;;

let pointer_arg_write_param pointer_data _oname params info _byref=
  let pname = params.pname in
  let ( varlist,  codebefore) =
    if params.pnull then
      let varlist = varlist_add info.varlist pointer_data.pd_typename ( "*"  ^  pname  ^  " = NULL") in
      (varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL") ,
       pa_null pname pointer_data.pd_typename pointer_data.pd_typecode)
    else
      let varlist = varlist_add info.varlist pointer_data.pd_typename ( "*"  ^  pname  ^  " = NULL") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname ^  " = NULL" ),
	pa_check pname pointer_data.pd_typename pointer_data.pd_typecode) in
  let arglist = pname in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname] in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let pointer_arg_write_return pointer_data ptype _ownsreturn info =
  let (flag, _new_type) = strip_type ptype in
  let (varlist, attrcodeafter) =
    if flag then
      ( varlist_add info.varlist pointer_data.pd_typename ( "*ret"),
	"  /* nspg_pointer_new handles NULL checking */\n"  ^
	"  return nspg_pointer_new("  ^  pointer_data.pd_typecode  ^  ", ret);")
    else
      (varlist_add info.varlist pointer_data.pd_typename ( "ret"),
       "  /* nspg_pointer_new handles NULL checking */\n"  ^
       "  return nspg_pointer_new("  ^  pointer_data.pd_typecode  ^  ", &ret);") in
  { info with varlist = varlist ; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let pointer_arg_attr_write_return pointer_data _objinfo _ownsreturn params info=
  let (flag, _new_type) = strip_type params.ptype in
  let (varlist, attrcodeafter) =
    if flag then
      ( varlist_add info.varlist pointer_data.pd_typename ( "*ret") ,
	"  /* nspg_pointer_new handles NULL checking */\n"  ^
	"  return nspg_pointer_new("  ^  pointer_data.pd_typecode  ^  ", ret);")
    else
      (varlist_add info.varlist pointer_data.pd_typename ( "ret") ,
       "  /* nspg_pointer_new handles NULL checking */\n"  ^
       "  return nspg_pointer_new("  ^  pointer_data.pd_typecode  ^  ", &ret);") in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}

let make_pointer_arg pointer_data =
  { argtype with
    write_param = pointer_arg_write_param pointer_data;
    (* attr_write_set = pointer_arg_attr_write_set pointer_data; *)
    write_return = pointer_arg_write_return pointer_data;
    attr_write_return = pointer_arg_attr_write_return  pointer_data;
    (* attr_free_fields = pointer_arg_attr_free_fields  pointer_data;
    attr_write_save = pointer_arg_attr_write_save;
    attr_write_load = pointer_arg_attr_write_load  pointer_data;
    attr_write_copy = pointer_arg_attr_write_copy  pointer_data;
    attr_write_info = pointer_arg_attr_write_info;
    attr_write_print = pointer_arg_attr_write_print  pointer_data;
    attr_write_init = pointer_arg_attr_write_init  pointer_data;
    attr_equal_fields = pointer_arg_attr_equal_fields  pointer_data;
    attr_write_defval = pointer_arg_attr_write_defval  pointer_data; *)
  }
;;

(* atom_arg *)

let atom name  =
  Printf.sprintf "  if ( nsp_gdk_atom_from_object(nsp_%s,&%s)==FAIL) return RET_BUG;\n"
    name name
;;

let atom_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let varlist = varlist_add info.varlist "GdkAtom" ( pname) in
  let varlist = varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL") in
  let codebefore = atom pname in
  let arglist = pname in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname] in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let atom_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "GdkAtom" ( "ret") in
  let varlist = varlist_add varlist "NspObject" ( "*nsp_ret") in
  let codeafter ="  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL)\n"  ^
    "    return RET_BUG;\n"  ^
    "  MoveObj(stack,1,nsp_ret);\n  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let atom_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "GdkAtom" ( "ret") in
  let attrcodeafter ="  return (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL);" in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let atom_arg=
  { argtype with
    write_param = atom_arg_write_param ;
    (* attr_write_set = atom_arg_attr_write_set ; *)
    write_return = atom_arg_write_return ;
    attr_write_return = atom_arg_attr_write_return  ;
    (* attr_free_fields = atom_arg_attr_free_fields  ;
    attr_write_save = atom_arg_attr_write_save;
    attr_write_load = atom_arg_attr_write_load  ;
    attr_write_copy = atom_arg_attr_write_copy  ;
    attr_write_info = atom_arg_attr_write_info;
    attr_write_print = atom_arg_attr_write_print  ;
    attr_write_init = atom_arg_attr_write_init  ;
    attr_equal_fields = atom_arg_attr_equal_fields  ;
    attr_write_defval = atom_arg_attr_write_defval  ; *)
  }
;;

(* gtype_arg *)

let gtype name =
  Printf.sprintf "  if ((%s = nspg_type_from_object(nsp_%s)) == FAIL)\
    \n      return RET_BUG;\n" name name
;;

let gtype_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let varlist = varlist_add info.varlist "GType" ( pname) in
  let varlist = varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL") in
  let codebefore = gtype pname in
  let arglist = pname in
  let info = add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname] in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let gtype_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "GType" ( "ret") in
  let codeafter ="  return nspg_type_wrapper_new(ret);" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let gtype_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "GType" ( "ret") in
  let attrcodeafter ="  return nspg_type_wrapper_new(ret);" in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let gtype_arg=
  { argtype with
    write_param = gtype_arg_write_param ;
    (* attr_write_set = gtype_arg_attr_write_set ; *)
    write_return = gtype_arg_write_return ;
    attr_write_return = gtype_arg_attr_write_return  ;
    (* attr_free_fields = gtype_arg_attr_free_fields  ;
    attr_write_save = gtype_arg_attr_write_save;
    attr_write_load = gtype_arg_attr_write_load  ;
    attr_write_copy = gtype_arg_attr_write_copy  ;
    attr_write_info = gtype_arg_attr_write_info;
    attr_write_print = gtype_arg_attr_write_print  ;
    attr_write_init = gtype_arg_attr_write_init  ;
    attr_equal_fields = gtype_arg_attr_equal_fields  ;
    attr_write_defval = gtype_arg_attr_write_defval  ; *)
  }
;;

(* gerror_arg *)

let handle_gerror name =
  Printf.sprintf "  if ( %s != NULL ) {\
    \n    Scierror(\"%%s: gtk error\\n%%s\\n\",NspFname(stack),%s->message);\
    \n    return RET_BUG;\n  }\n" name name 
;;

let gerror_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let varlist = varlist_add info.varlist "GError" ( "*"  ^  pname  ^  " = NULL")  in
  let arglist = "&"  ^  pname in
  let codeafter = handle_gerror pname in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codeafter = codeafter :: info.codeafter;
  }
;;

let gerror_arg=
  { argtype with
    write_param = gerror_arg_write_param ;
  }
;;

(* gtk_tree_path_arg *)

let normal name =
  Printf.sprintf "  %s = nsp_gtk_tree_path_from_nspobject(nsp_%s);\
   \n  if (!%s) {\
   \n      Scierror( \"Error: could not convert %s to a GtkTreePath\\n\");\
   \n      return RET_BUG;\
   \n  }\n" name name name name
;;

let null name =
  Printf.sprintf "  if ( ! IsNone(nsp_%s)) {\
   \n      %s = nsp_gtk_tree_path_from_nspobject(nsp_%s);\
   \n      if (!%s) {\
   \n          Scierror( \"Error: could not convert %s to a GtkTreePath\\n\");\
   \n          return RET_BUG;\
   \n      }\
   \n  }\n" name name name name name
;;

(*
let null1 name =
  Printf.sprintf "  if (PyTuple_Check(nsp_%s))\
   \n      %s = nsp_gtk_tree_path_from_nspobject(nsp_%s);\
   \n  else if ( !IsNone(nsp_%s)) {\
   \n      Scierror( \"Error: %s should be a GtkTreePath or None\\n\");\
   \n      return RET_BUG;\
   \n  }\n" name name name name name
;;
*)

let freepath name =
  Printf.sprintf "  if (%s)\
    \n      gtk_tree_path_free(%s);\n" name name
;;

let gtk_tree_path_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let (varlist, info, arglist,codebefore ) =
    if params.pnull then
      let varlist = varlist_add info.varlist "GtkTreePath" ( "*"  ^  pname  ^  " = NULL") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL"),
	add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname],
	pname,
	null  pname)
    else
      let varlist = varlist_add info.varlist "GtkTreePath" ( "*"  ^  pname) in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname),
	add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname],
        pname,
	normal pname) in
  let codeafter = freepath pname in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
    codeafter = codeafter :: info.codeafter;
   }

;;

let gtk_tree_path_arg_write_return _ptype ownsreturn info =
  let varlist = varlist_add info.varlist "GtkTreePath" ( "*ret") in
  let codeafter =
    if ownsreturn then
      "  if (ret) {\n"
      ^  "      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n"
      ^  "      gtk_tree_path_free(ret);\n"
      ^  "      return nsp_ret;\n"
      ^  "  }\n"
      ^  "  return RET_BUG;"
    else
      "  if (ret) {\n"
      ^"      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n"
      ^"      return nsp_ret;\n"
      ^"  }\n"
      ^"  return RET_BUG;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let gtk_tree_path_arg_attr_write_return _objinfo ownsreturn _params info=
  let varlist = varlist_add info.varlist "GtkTreePath" ( "*ret") in
  let attrcodeafter =
    if ownsreturn then
      "  if (ret) {\
     \n      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\
     \n      gtk_tree_path_free(ret);\
     \n      return nsp_ret;\
     \n  }\
     \n  return NULL;"
    else
     "  if (ret) {\
    \n      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\
    \n      return nsp_ret;\
    \n  }\
    \n  return NULL;" in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let gtk_tree_path_arg=
  { argtype with
    write_param = gtk_tree_path_arg_write_param ;
    (* attr_write_set = gtk_tree_path_arg_attr_write_set ; *)
    write_return = gtk_tree_path_arg_write_return ;
    attr_write_return = gtk_tree_path_arg_attr_write_return  ;
    (* attr_free_fields = gtk_tree_path_arg_attr_free_fields  ;
    attr_write_save = gtk_tree_path_arg_attr_write_save;
    attr_write_load = gtk_tree_path_arg_attr_write_load  ;
    attr_write_copy = gtk_tree_path_arg_attr_write_copy  ;
    attr_write_info = gtk_tree_path_arg_attr_write_info;
    attr_write_print = gtk_tree_path_arg_attr_write_print  ;
    attr_write_init = gtk_tree_path_arg_attr_write_init  ;
    attr_equal_fields = gtk_tree_path_arg_attr_equal_fields  ;
    attr_write_defval = gtk_tree_path_arg_attr_write_defval  ; *)
  }
;;

(* gdk_rectangle_pointer_arg *)

let normal name =
  Printf.sprintf "  if (!nsp_gdk_rectangle_from_object(nsp_%s, &%s))\
    \n      return RET_BUG;\n" name name
;;

let null name =
  Printf.sprintf "  if (nsp_%s == NULL)\
   \n      %s = NULL;\
   \n  else if (nsp_gdk_rectangle_from_object(nsp_%s, &%s_rect))\
   \n      %s = &%s_rect;\
   \n  else\
   \n          return RET_BUG;\n" name name name name  name  name
;;

let gdk_rectangle_pointer_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let (varlist, info, arglist,codebefore ) =
    if params.pnull then
      let varlist = varlist_add info.varlist "GdkRectangle" ( pname  ^  "_rect = { 0, 0, 0, 0 }") in
      let varlist = varlist_add varlist "GdkRectangle" ( "*"  ^  pname) in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname  ^  " = NULL"),
	add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname],
	pname,
	null pname)
    else
      let varlist = varlist_add info.varlist "GdkRectangle" ( pname  ^  " = { 0, 0, 0, 0 }") in
      ( varlist_add varlist "NspObject" ( "*nsp_"  ^  pname),
	add_parselist info params.pvarargs "obj" ["&nsp_"  ^  pname] [pname],
	"&"  ^  pname,
	normal pname) in
  { info with arglist = arglist :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore;
  }
;;

let gdk_rectangle_pointer_arg=
  { argtype with
    write_param = gdk_rectangle_pointer_arg_write_param ;
  }
;;

(* gdk_rectangle_arg *)

let gdk_rectangle_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "GdkRectangle" ( "ret") in
  let varlist = varlist_add varlist "NspObject" ( "*nsp_ret") in
  let codeafter ="  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE,&ret, TRUE, TRUE,NULL))==NULL)\n"
    ^ "    return RET_BUG;\n  MoveObj(stack,1,nsp_ret);\n  return 1;" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let gdk_rectangle_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "GdkRectangle" ( "ret") in
  let attrcodeafter ="  return (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &ret, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkrectangle);" in
  { info with varlist = varlist; attrcodeafter = attrcodeafter :: info.attrcodeafter ;}
;;

let gdk_rectangle_arg=
  { argtype with
    write_return = gdk_rectangle_arg_write_return ;
    attr_write_return = gdk_rectangle_arg_attr_write_return  ;
  }
;;

(* nsp_glist_arg *)

let nsp_glist_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspList" ("*nsp_"  ^  pname)
    | Some x ->
	varlist_add info.varlist "NspList" ("*nsp_"  ^  pname  ^ " = "  ^ x) in
  let varlist = varlist_add varlist "GList" ("*" ^ pname) in
  let codebefore =
    "  " ^ pname ^ "=nsp_glist_from_nsplist(stack,nsp_" ^  pname ^ ");\n"
    ^ "  if (" ^ pname ^ "== NULL) return RET_BUG;\n" in
  let info = add_parselist info params.pvarargs "list" ["&nsp_"  ^  pname] [pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_glist_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "GList"  "*ret" in
  let varlist = varlist_add varlist "GList"  "*tmp" in
  let varlist = varlist_add varlist "NspList"  "*nsp_list" in
  let codeafter =
    "  NSP_LIST_FROM_GLIST(ret,nspgobject_new(\"lel\",(GObject *)tmp->data),g_list_free);\n" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let nsp_glist_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "GList"  "*ret" in
  let varlist = varlist_add varlist "GList"  "*tmp" in
  let varlist = varlist_add varlist "NspList"  "*nsp_list" in
  let codeafter =
    "  NSP_OBJ_LIST_FROM_GLIST(ret,nspgobject_new(\"lel\",(GObject *)tmp->data),g_list_free);\n" in
  { info with varlist = varlist; codeafter = codeafter :: info.codeafter ;}
;;

let nsp_glist_arg=
  { argtype with
    write_param = nsp_glist_arg_write_param ;
    (* attr_write_set = nsp_glist_arg_attr_write_set ; *)
    write_return = nsp_glist_arg_write_return ;
    attr_write_return = nsp_glist_arg_attr_write_return  ;
  }
;;

(* nsp_gslist_arg *)

let nsp_gslist_arg_write_param _oname params info _byref=
  let pname = params.pname in
  let varlist =
    match params.pdflt with
    | None ->
	varlist_add info.varlist "NspList" ("*nsp_"  ^  pname)
    | Some x ->
      varlist_add info.varlist "NspList" ("*nsp_"  ^  pname  ^ " = "  ^ x) in
  let varlist = varlist_add varlist "GSList" ("*" ^ pname) in
  let codebefore =
    "  " ^ pname ^ "=nsp_gslist_from_nsplist(stack,nsp_" ^  pname ^ ");\n"
    ^ "  if (" ^ pname ^ "== NULL) return RET_BUG;\n" in
  let info = add_parselist info params.pvarargs "list" ["&nsp_"  ^  pname] [pname] in
  { info with
    arglist = params.pname :: info.arglist;
    varlist = varlist;
    codebefore = codebefore :: info.codebefore ;
  }
;;

let nsp_gslist_arg_write_return _ptype _ownsreturn info =
  let varlist = varlist_add info.varlist "GSList"  "*ret" in
  let varlist = varlist_add varlist "GSList"  "*tmp" in
  let varlist = varlist_add varlist "NspList"  "*nsp_list" in
  let codeafter =
    "  NSP_LIST_FROM_GLIST(ret,nspgobject_new(\"lel\",(GObject *)tmp->data),g_slist_free);\n" in
  { info with varlist = varlist ; codeafter = codeafter :: info.codeafter ;}
;;

let nsp_gslist_arg_attr_write_return _objinfo _ownsreturn _params info=
  let varlist = varlist_add info.varlist "GSList"  "*ret" in
  let varlist = varlist_add varlist "GSList"  "*tmp" in
  let varlist = varlist_add varlist "NspList"  "*nsp_list" in
  let codeafter =
    "  NSP_OBJ_LIST_FROM_GLIST(ret,nspgobject_new(\"lel\",(GObject *)tmp->data),g_slist_free);\n" in
  { info with varlist = varlist; codeafter = codeafter :: info.codeafter ;}
;;

let nsp_gslist_arg=
  { argtype with
    write_param = nsp_gslist_arg_write_param ;
    (* attr_write_set = nsp_gslist_arg_attr_write_set ; *)
    write_return = nsp_gslist_arg_write_return ;
    attr_write_return = nsp_gslist_arg_attr_write_return  ;
  }
;;


(* tests
 *-------------------------
 *)
(*
let print_vars ppf varlist =
  let iter_key  key value =
    Printf.fprintf ppf "%s %s\n" key value in
  Hashtbl.iter iter_key  varlist
;;
*)

type parser= {
    mutable objects: object_rec list;
    mutable interfaces: object_rec list;
    mutable structures: object_rec list;
    mutable boxes: object_rec list;
    mutable pointers: object_rec list;
    mutable enums: enum list;
    mutable functions: function_obj list;
  }
;;

let parser =
  {
   objects = [];
   interfaces = [];
   structures = [];
   pointers =[];
   boxes = [];
   functions = [];
   enums = [];
  }
;;

let register_parser =
  {
   objects = [];
   interfaces = [];
   structures = [];
   pointers =[];
   boxes = [];
   functions = [];
   enums = [];
  }
;;

let of_bindings bindings =
  let l = List.length bindings in
  let t = Hashtbl.create l in
  List.iter (fun (k, v) -> Hashtbl.add t k v) bindings;
  t
;;

let matcher_hash = of_bindings [
  (* none *)
  "none", nonearg;
  (* string *)
  "char*", stringarg;
  "gchar*", stringarg;
  "const-char*", stringarg;
  "char-const*", stringarg;
  "const-gchar*", stringarg;
  "gchar-const*", stringarg;
  "string", stringarg;
  "static_string", stringarg;
  (* uchar *)
  "unsigned-char*", uchararg;
  "const-guchar*", uchararg;
  "guchar*", uchararg;
  (* chararg *)
  "char", chararg;
  "gchar", chararg;
  "guchar", chararg;
  (* GUniCharArg *)
  "gunichar", gunichararg;
  (* IntArg *)
  "int", intarg;
  "gint", intarg;
  "guint", intarg;
  "short", intarg;
  "gshort", intarg;
  "gushort", intarg;
  "long", intarg;
  "glong", intarg;
  "gsize", intarg;
  "gssize", intarg;
  "guint8", intarg;
  "gint8", intarg;
  "guint16", intarg;
  "gint16", intarg;
  "gint32", intarg;
  "guint32", ulong_arg; (* *)
  (* double *)
  "double", double_arg;
  "gdouble", double_arg;
  "float", double_arg;
  "gfloat", double_arg;
  (* boolean *)
  "gboolean", boolean_arg;
  "Boolean", boolean_arg;
  "boolean", boolean_arg;

  "NspMatrix*", (make_nsp_generic_arg_mat
		  (init_nsp_generic_data "NspMatrix" "Matrix" "Mat" "mat"));

  "NspBMatrix*", (make_nsp_generic_arg_bmat
		   (init_nsp_generic_data "NspBMatrix" "BMatrix" "BMat" "bmat"));

  "NspSMatrix*", (make_nsp_generic_arg_smat
		   (init_nsp_generic_data "NspSMatrix" "SMatrix" "SMat" "smat"));

  "NspPMatrix*", (make_nsp_generic_arg_pmat
		   (init_nsp_generic_data "NspPMatrix" "PMatrix" "PMat" "pmat"));

  "NspList*", (make_nsp_generic_arg_list
		(init_nsp_generic_data "NspList" "List" "List" "list"));
  "NspSpColMatrix*", (make_nsp_generic_arg_spcol
		       (init_nsp_generic_data "NspSpColMatrix" "SpColMatrix" "SpColMat" "sp"));
  "NspCells*", (make_nsp_generic_arg_cells
		 (init_nsp_generic_data "NspCells" "Cells" "Cells" "ce"));

  "mat", nsp_mat_arg;
  "matcopy", nsp_mat_copy_arg;
  "double[]", nsp_double_array_arg;
  "const double[]", nsp_double_array_copy_arg;
  "doubleC[]", nsp_complex_array_arg;
  "const doubleC[]" , nsp_complex_array_copy_arg;
  "int[]", nsp_int_array_arg;
  "const int[]" , nsp_int_array_copy_arg;
  "boolean[]" , nsp_bool_array_arg;
  "const boolean[]" , nsp_bool_array_copy_arg;
  "double*" , double_pointer_arg;
  "gdouble*", double_pointer_arg;
  "int*" , int_pointer_arg;
  "gint*" , int_pointer_arg;
  "int64*"  , int64_pointer_arg;
  "gint64*"  , int64_pointer_arg;
  "boolean*", bool_pointer_arg;
  "gboolean*", bool_pointer_arg;
  "time_t",  time_t_arg;
  "gulong" , ulong_arg;
  "size_t"  , ulong_arg;
  "gint64" , int64_arg;
  "long-long" , int64_arg;
  "guint64" , uint64_arg;
  "unsigned-long-long" , uint64_arg;
  "FILE*", file_arg;
  "void*" , void_pointer_arg ;
  (* NspObject pointer *)
  "NspObject*", (make_nsp_object_arg
		   (init_nsp_generic_data "NspObject" "Object" "Obj" "object"));
  "GSList*", nsp_gslist_arg;
  "GList*", nsp_glist_arg;
  "GdkAtom", atom_arg;
  "GType", gtype_arg;
  "GtkType", gtype_arg;
  "GError**", gerror_arg;
  "GdkRectangle*", gdk_rectangle_pointer_arg;
  "GtkAllocation*", gdk_rectangle_pointer_arg;
  "GdkRectangle", gdk_rectangle_arg;
  "GdkNativeWindow", ulong_arg;
  "@@TreePath", gtk_tree_path_arg; (* example ? *)
  "@@CustomBoxed", (make_custom_boxed_arg
                      (init_custom_boxed_data "getter" "checker" "new_s" "type_s"));
  "gchar**", string_array_arg;
  "const-gchar**", const_string_array_arg; 
  "char**", string_array_arg;
]
;;

let matcher_get str =
  (* special case for event matcher *)
  (* Say.warning (Printf.sprintf "Searching matcher for %s" str ); *)
  let pat = "GdkEvent" in
  let n_pat = (String.length pat) in
  let n = (String.length str) in
  let str =
    (
     if n > n_pat && (String.sub str 0 n_pat) = pat && str.[n-1] = '*' then
       (
	pat ^ "*"
       )
     else
       str) in
  if Hashtbl.mem matcher_hash str then
    Hashtbl.find matcher_hash str
  else
    Say.fatal_error ("matcher not found for " ^ str)
;;

(* register an object which can be used as arguments *)
(* we assume that for interfaces the or_parent is an empty string *)

let register_object object_rec =
  let od =
    {
     od_objname = object_rec.or_c_name;
     od_name = object_rec.or_name;
     od_cast = object_rec.or_c_name;
     od_parent = object_rec.or_parent;
     od_nsp_arg_type =  object_rec.or_typecode;
     od_shortname = object_rec.or_name;
     od_shortname_uc = (String.uppercase_ascii object_rec.or_name);
   } in
  let arg =
    (* if od.od_parent = "GObject" then  *)
    if check_gtk_class object_rec then
      (
       Say.debug
	 (Printf.sprintf "Register %s as a GObject" object_rec.or_c_name);
       make_gtk_object_arg od
      )
    else
      (
       Say.debug
	 (Printf.sprintf "Register %s %s" object_rec.or_c_name
	    (object_rec.or_c_name ^ "*"));
	  make_object_arg od)
  in
  Hashtbl.add matcher_hash object_rec.or_c_name arg;
  Hashtbl.add matcher_hash (object_rec.or_c_name ^ "*") arg;
;;

(* register an enum or a flag *)

let register_enum enum_rec =
   let ed =
   {
     enum_name =  enum_rec.e_c_name;
     enum_typecode=  if enum_rec.e_typecode = "" then
       "G_TYPE_NONE" else enum_rec.e_typecode;
   } in
 let arg =
   if enum_rec.is_enum then
     make_enum_arg ed
   else
     make_flag_arg ed in
  (* Printf.printf "Debug: Register %s" enum_rec.e_c_name;  *)
  Hashtbl.add matcher_hash enum_rec.e_c_name arg;
;;

(* register a structure *)

let register_struct object_rec =
  let arg = make_struct_arg object_rec in
  Hashtbl.add matcher_hash object_rec.or_c_name arg;
  Hashtbl.add matcher_hash (object_rec.or_c_name ^ "*")  arg;
  Hashtbl.add matcher_hash ("const-" ^ object_rec.or_c_name ^ "*")  arg;
;;

(* register boxed *)

let register_get_name object_rec =
  if object_rec.or_module = "EGtk" then
    object_rec.or_name
  else
    object_rec.or_c_name
;;

let register_boxed object_rec =
  let bd = {
    bd_typename =   object_rec.or_c_name;
    bd_typecode =   object_rec.or_typecode;
  } in
  let arg = make_boxed_arg bd in
  let name = register_get_name object_rec in
  match name with
  | "GdkRectangle" ->
      (* do not register GdkRectangle as boxed it is already in stringarg *)
      ()
  | _ ->
      Say.debug (Printf.sprintf "Register boxed %s" name);
      Hashtbl.add matcher_hash name arg;
      Hashtbl.add matcher_hash (name ^ "*")  arg;
      Hashtbl.add matcher_hash ("const-" ^ name ^ "*")  arg;
;;

(* register pointer *)

let register_pointer object_rec =
  let pd = {
    pd_typename =   object_rec.or_c_name;
    pd_typecode =   object_rec.or_typecode;
  } in
  let arg = make_pointer_arg pd in
  Hashtbl.add matcher_hash object_rec.or_c_name arg;
  Hashtbl.add matcher_hash (object_rec.or_c_name ^ "*")  arg;
  Hashtbl.add matcher_hash ("const-" ^ object_rec.or_c_name ^ "*")  arg;
;;

(* register the types *)

let register_types parser =
  (* used for objects and interfaces *)
  List.iter (fun obj -> register_object obj) parser.interfaces;
  List.iter (fun obj -> register_object obj) parser.objects;
  List.iter (fun obj -> register_struct obj) parser.structures;
  List.iter (fun obj -> register_boxed obj) parser.boxes;
  List.iter (fun obj -> register_pointer obj) parser.pointers;
  List.iter (fun obj -> register_enum obj) parser.enums;
  register_object
	{
	  or_name= "GObject";
	  or_module= "";
	  or_parent= "";
	  or_c_name= "GObject";
	  or_typecode= "G_TYPE_OBJECT";
	  or_byref= false;
	  or_kind= Object;
	  or_fields= [];
	  or_implements= [];
	  or_copy_func= "";
	  or_release_func="";
	  or_availability ="";
	}
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
