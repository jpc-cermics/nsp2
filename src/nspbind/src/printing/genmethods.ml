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
  "  $(setreturn)$(cname)($(self)$(arglist));\n" ^
  "$(codeafter)\n" ^
  "}\n\n"
;;

let method_tmpl_boxed = 
  "static int _wrap_$(cname)($(typename) *self,Stack stack,int rhs,int opt,int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "  $(setreturn)$(cname)(NSP_GBOXED_GET(self, $(typename_nn))$(arglist));\n" ^
  "$(codeafter)\n" ^
  "}\n\n"
;;

let method_tmpl objinfo = 
  match objinfo.or_kind with 
  | Boxed ->  method_tmpl_boxed
  | Pointer 
  | Interface 
  | Object 
  | Struct ->  method_tmpl_std
;;

let constructor_tmpl_std =
  "static int\n" ^
  "_wrap_$(typename_dc)_new(Stack stack, int rhs, int opt, int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "  GObject *ret; NspObject *nsp_ret;\n" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "  if ((ret = (GObject *)$(cname)($(arglist)))== NULL) return RET_BUG;\n" ^
  "$(codeafter)\n" ^
  "  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\n" ^
  "  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_$(typename_dc) );\n " ^
  "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
  "  MoveObj(stack,1,nsp_ret);\n" ^
  "  return 1;\n" ^
  "}\n\n"
;;

let constructor_tmpl_boxed =
  "static int\n" ^
  "_wrap_$(typename_dc)_new(Stack stack, int rhs, int opt, int lhs)\n" ^
  "{\n" ^
  "$(varlist)" ^
  "  GObject *ret; NspObject *nsp_ret;\n" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "  if ((ret = (GObject *)$(cname)($(arglist)))== NULL) return RET_BUG;\n" ^
  "$(codeafter)\n" ^
  "  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\n" ^
  "  nsp_ret = (NspObject *) gboxed_create(NVOID,$(typecode), ret,TRUE,TRUE,(NspTypeBase *) nsp_type_$(typename_dc));\n" ^
  "  if ( nsp_ret == NULL) return RET_BUG;\n" ^
  "  MoveObj(stack,1,nsp_ret);\n" ^
  "  return 1;\n" ^
  "}\n\n"
;;

let constructor_tmpl objinfo = 
  match objinfo.or_kind with 
  | Boxed ->   constructor_tmpl_boxed
  | Pointer 
  | Interface 
  | Object 
  | Struct ->   constructor_tmpl_std
;;

(* two methods can be implemented with the same c function 
 *  we have to take care not to insert the function twice 
 *)

let written_overrides_table = Hashtbl.create 256;;

let write_method objinfo is_gtk_class  meth template handle_return is_method = 
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
	     (Printf.sprintf "Found %s in not override \n" method_name1);
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
	      Say.debug "matcher found";
	      ( try 
		let code, _methflags = 
		  Genfunctions.write_function_wrapper objinfo.or_c_name objinfo.or_byref meth template
		    handle_return is_method substdict in 
		File.write_string code;
		(Printf.sprintf "  {\"%s\",(nsp_method *) %s},\n" (fixname meth.f_name) ("_wrap_"  ^ meth.f_c_name) , false)
	      with _ -> 
		Say.debug "to be checked a matcher failed";
		("", true)
	       )
	     )
	   else
	     (
	      Say.warning 
		(Printf.sprintf "Failed to generate %s: matcher missing for %s" meth.f_name str);
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
	let xval, flag = (write_method objinfo is_gtk_class x (method_tmpl objinfo) true true) in
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
      let _str, flag = (write_method objinfo is_gtk_class x (constructor_tmpl objinfo) false false) in 
      if flag then 
	(
	 Hashtbl.add failed_tbl ((String.lowercase x.is_constructor_of) ^ "_new" ) "_"
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
