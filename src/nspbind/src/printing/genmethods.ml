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

open Stringarg;;

let method_tmpl_std =
  "static int _wrap_$(cname)($(typename) *self,Stack stack,int rhs,int opt,int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "}\n\n"
;;

let method_call_tmpl_std =
  "    $(setreturn)$(cname)($(self)$(arglist));\n"
;;

let method_tmpl_boxed =
  "static int _wrap_$(cname)($(typename) *self,Stack stack,int rhs,int opt,int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "}\n\n"
;;

let method_call_tmpl_boxed =
  "  $(setreturn)$(cname)(NSP_GBOXED_GET(self, $(typename_nn))$(arglist));\n"
;;

let method_tmpl objinfo =
  match objinfo.or_kind with
  | Boxed ->  method_tmpl_boxed
  | Pointer
  | Interface
  | Object
  | Struct ->  method_tmpl_std
;;

let method_call_tmpl objinfo =
  match objinfo.or_kind with
  | Boxed ->  method_call_tmpl_boxed
  | Pointer
  | Interface
  | Object
  | Struct ->  method_call_tmpl_std
;;

(*
 * nspgobject_new could be used instead of gobject_create in constructor_tmpl_std_gtkclass
 * the main advantage being that it creates the most specialized object and this is better
 * This is mostly done in override and thus it should be better to make it automatically 
 *)

let constructor_tmpl_std =
  "static int\n" ^
  "_wrap_$(cname) (Stack stack, int rhs, int opt, int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "  void *ret; NspObject *nsp_ret;\n" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\n" ^
  "  nsp_ret =(NspObject*) nsp_$(typename_dc)_create(NVOID,ret,(NspTypeBase *) nsp_type_$(typename_dc));\n" ^
  "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
  "  MoveObj(stack,1,nsp_ret);\n" ^
  "  return 1;\n" ^
  "}\n\n"
;;

let constructor_call_tmpl_std =
  "  if ((ret = $(cname)($(arglist)))== NULL) return RET_BUG;\n"
;;

let constructor_tmpl_std_gtkclass =
  "static int\n" ^
  "_wrap_$(cname) (Stack stack, int rhs, int opt, int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "  GObject *ret; NspObject *nsp_ret;\n" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\n" ^
  "  /* prefer most specialized class than the one specified indef file\n" ^
  "   * nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_$(typename_dc) );\n " ^
  "   */\n" ^
  "  nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *)ret);\n" ^
  "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
  "  MoveObj(stack,1,nsp_ret);\n" ^
  "  return 1;\n" ^
  "}\n\n"
;;

let constructor_call_tmpl_std_gtkclass =
  "  if ((ret = (GObject *)$(cname)($(arglist)))== NULL) return RET_BUG;\n"
;;

let constructor_tmpl_boxed =
  "static int\n" ^
  "_wrap_$(cname) (Stack stack, int rhs, int opt, int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "  GObject *ret; NspObject *nsp_ret;\n" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\n" ^
  "  nsp_ret = (NspObject *) gboxed_create(NVOID,$(typecode), ret,TRUE,TRUE,(NspTypeBase *) nsp_type_$(typename_dc));\n" ^
  "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
  "  MoveObj(stack,1,nsp_ret);\n" ^
  "  return 1;\n" ^
  "}\n\n"
;;

let constructor_call_tmpl_boxed =
  "  if ((ret = (GObject *)$(cname)($(arglist)))== NULL) return RET_BUG;\n"
;;

let constructor_tmpl objinfo is_gtk_class =
  match objinfo.or_kind with
  | Boxed ->   constructor_tmpl_boxed
  | Pointer
  | Interface
  | Object
  | Struct ->
      if is_gtk_class then
	constructor_tmpl_std_gtkclass
      else
	constructor_tmpl_std
;;

let constructor_call_tmpl objinfo is_gtk_class =
  match objinfo.or_kind with
  | Boxed ->   constructor_call_tmpl_boxed
  | Pointer
  | Interface
  | Object
  | Struct ->
      if is_gtk_class then
	constructor_call_tmpl_std_gtkclass
      else
	constructor_call_tmpl_std
;;

(* two methods can be implemented with the same c function
 *  we have to take care not to insert the function twice
 *)

let written_overrides_table = Hashtbl.create 256;;

let write_method objinfo is_gtk_class  meth template template_call handle_return is_method =
  let fixname x = x in
  let method_name1 = meth.f_c_name in
  let method_name2 = objinfo.or_name  ^ "."  ^ meth.f_c_name in

  if Overrides.is_ignored method_name1 then
    ("", false)
  else
    (
     if Overrides.is "override" method_name1 then
       (
	Say.debug
	  (Printf.sprintf "Found %s (%s) in override (step 1)" method_name1 ("_wrap_"  ^ meth.f_c_name)) ;
	if not (Hashtbl.mem written_overrides_table method_name1) then
	  (
	   File.write_override "override" method_name1;
	   Hashtbl.add written_overrides_table method_name1 "_";
	  );
	File.write_string "\n\n";
	(
	 Printf.sprintf "  {\"%s\",(nsp_method *) %s},\n" (fixname meth.f_name) ("_wrap_"  ^ meth.f_c_name) ,
	 false)
       )
     else
       (
	if Overrides.is "override" method_name2 then
	  (
	    Say.debug
	      (Printf.sprintf "Found %s in override (step 2)" method_name2);
	    File.write_override "override"  method_name2;
	    File.write_string "\n\n";
	    (Printf.sprintf "  {\"%s\",(nsp_method *) %s},\n" (fixname meth.f_name) ("_wrap_"  ^ meth.f_c_name) ,
	     false)
	   )
	else
	  (
	   Say.debug
	     (Printf.sprintf "method %s is not overriden" method_name1);
	   let substdict=  Hashtbl.create 256 in
	   Hashtbl.add substdict "typename"  objinfo.or_c_name;
	   Hashtbl.add substdict "typename_nn"  objinfo.or_name;
	   Hashtbl.add substdict "typename_dc" (String.lowercase objinfo.or_name);
	   Hashtbl.add substdict "typecode" objinfo.or_typecode;

	   let cast =
	     if is_gtk_class (* XXXX objinfo.or_parent = "GObject"*) then
	       (
		(Printf.sprintf "%s(self->obj)"
		   (Str.global_replace (Str.regexp "_TYPE_") "_" objinfo.or_typecode))
	       )
	     else
	       "self" in
	   Hashtbl.add substdict "self" cast;
	   let ret =
	     match meth.ret with
	     | Some x -> x
	     | None -> "none" in
	   let (test, str) =  Genfunctions.check_matchers meth.params ret in
	   if test then
	     (
	      Say.debug "all matchers for params and returned value were found";
	      ( try
		let code, _methflags =
		  Genfunctions.write_function_wrapper objinfo.or_c_name objinfo.or_byref meth template
		    template_call handle_return is_method substdict in
		File.write_string code;
		Say.debug (Printf.sprintf "code for method %s generated" meth.f_c_name);
		(Printf.sprintf "  {\"%s\",(nsp_method *) %s},\n" (fixname meth.f_name) ("_wrap_"  ^ meth.f_c_name) , false)
	      with _ ->
		Say.debug (Printf.sprintf "Warning: failed to generate method %s" meth.f_c_name);
		("", true)
	       )
	     )
	   else
	     (
	      Say.warning
		(Printf.sprintf "Warning: failed to generate method %s: matcher missing for %s" meth.f_name str);
	      ("",true)
	     )
	  )
       )
    )
;;

let find_methods objinfo =
  let find_method func accu =
    if func.is_method && func.of_object = objinfo.or_name then
      func :: accu
    else
      accu in
  List.fold_right find_method Stringarg.parser.functions []
;;

let write_methods objinfo is_gtk_class =
  let methods = find_methods objinfo in
  let methods =
    List.fold_right
      (fun x value ->
	let xval, flag = (write_method objinfo is_gtk_class x (method_tmpl objinfo) (method_call_tmpl objinfo) true true) in
        if flag then value else xval :: value )
      methods [] in
  let lower_name = (String.lowercase objinfo.or_name) in
  if List.length methods > 0 then
    let methoddefs = Printf.sprintf "%s_methods"  lower_name in
    let methods = List.rev ("  { NULL, NULL}\n" :: methods) in
    File.write_string (Printf.sprintf "static NspMethods %s[] = {\n"  methoddefs);
    List.iter ( fun x -> File.write_string x ) methods;
    File.write_string "};\n\n";
    File.write_string
      (Printf.sprintf
	 "static NspMethods *%s_get_methods(void) { return %s;};\n" lower_name  methoddefs)
  else
    (
      let _methoddefs = Printf.sprintf "%s_methods"  objinfo.or_c_name in
      File.write_string
	(Printf.sprintf "static NspMethods *%s_get_methods(void) { return NULL;};\n"
	   lower_name));
;;

let find_constructors objinfo =
  let find_constructor func accu =
    if func.is_constructor_of = objinfo.or_name then
      func :: accu
    else
      accu in
  List.fold_right find_constructor Stringarg.parser.functions []
;;

let write_constructors objinfo is_gtk_class failed_tbl =
  let constructors = find_constructors objinfo in
  List.iter
    (fun x ->
      let _str, flag = (write_method objinfo is_gtk_class x (constructor_tmpl objinfo is_gtk_class)
			  (constructor_call_tmpl objinfo is_gtk_class)  false false) in
      if flag then
	(
	 Hashtbl.add failed_tbl x.f_c_name  "_"
	);
    )
    constructors;
  constructors;
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
