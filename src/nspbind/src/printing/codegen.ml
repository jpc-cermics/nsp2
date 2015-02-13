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
open Build;;

(* util *)

let get_override_pattern keyword name = 
  if Overrides.is keyword name then 
    let code, lineno = Overrides.get keyword name in 
    let code = 
      Str.global_replace (Str.regexp_string "%(ret)s") "$(ret)" code in 
    Printf.sprintf "#line %d \"%s\"$(nl)%s$(line)$(nl)" 
      lineno (File.get_override_c_file_name ()) code 
  else
    "" 
;;

(*---------------- insert method declarations ------------------- *)

let type_tmpl_top1 = 
  "  /* object methods redefined for $(typename_dc) */ \n" ^
  "\n" ^
  "  top->pr = (print_func *) nsp_$(typename_dc)_print;\n" ^
  "  top->dealloc = (dealloc_func *) nsp_$(typename_dc)_destroy;\n" ^
  "  top->copy  =  (copy_func *) nsp_$(typename_dc)_copy;\n" ^
  "  top->size  = (size_func *) nsp_$(typename_dc)_size;\n" ^
  "  top->s_type =  (s_type_func *) nsp_$(typename_dc)_type_as_string;\n" ^
  "  top->sh_type = (sh_type_func *) nsp_$(typename_dc)_type_short_string;\n" ^
  "  top->info = (info_func *) nsp_$(typename_dc)_info;\n" ^
  "  /* top->is_true = (is_true_func  *) nsp_$(typename_dc)_is_true; */\n"
;;

let type_tmpl_top2 = 
  "  top->get_from_obj = (get_from_obj_func *) nsp_$(typename_dc)_object;\n" ^
  "  top->eq  = (eq_func *) nsp_$(typename_dc)_eq;\n" ^
  "  top->neq  = (eq_func *) nsp_$(typename_dc)_neq;\n" ^
  "  top->save  = (save_func *) nsp_$(typename_dc)_xdr_save;\n" ^
  "  top->load  = (load_func *) nsp_$(typename_dc)_xdr_load;\n" ^
  "  top->create = (create_func*) int_$(typename_dc)_create;\n" ^
  "  top->latex = (print_func *) nsp_$(typename_dc)_latex;\n" ^
  "  top->full_copy = (copy_func *) nsp_$(typename_dc)_full_copy;\n" ^
  "\n" ^
  "  /* specific methods for $(typename_dc) */\n" ^
  "\n" ^
  "  type->init = (init_func *) init_$(typename_dc);\n" ^
  "\n"
;;


let type_tmpl_top3 = 
  "  /* object methods redefined for $(typename_dc) */ \n" ^
  "\n" ^
  "  top->s_type =  (s_type_func *) nsp_$(typename_dc)_type_as_string;\n" ^
  "  top->sh_type = (sh_type_func *) nsp_$(typename_dc)_type_short_string;\n" ^
  "  /* top->create = (create_func*) int_$(typename_dc)_create;*/\n" ^
  "\n" ^
  "  /* specific methods for $(typename_dc) */\n" ^
  "\n" ^
  "  type->init = (init_func *) init_$(typename_dc);\n" ^
  "\n"
;;


let insert_loop_code typename  substdict = 
  if Overrides.is "override-loop" typename then 
    File.write_override "override-loop" typename
  else
    File.write_string 
      (Printf.sprintf 
	 "  /* top->loop =(loop_func *) nsp_%s_loop;*/\n" 
	 (Hashtbl.find substdict "typename_dc"))
;;

let insert_path_extract typename _substdict = 
  if Overrides.is "override-path-extract" typename then
    File.write_override "override-path-extract" typename
  else
    File.write_string 
      "  top->path_extract = (path_func *)  object_path_extract;\n"
;;

let insert_methods typename _objinfo substdict =
  File.write_substitute_pattern type_tmpl_top1 substdict;
  (* insert loop override code *)
  insert_loop_code typename  substdict;
  (* insert path extract override code *)
  insert_path_extract typename  substdict;
  (* insert type override code  *)
  File.write_substitute_pattern type_tmpl_top2  substdict;
;;

let insert_methods_gtk _typename _objinfo substdict =
  File.write_substitute_pattern type_tmpl_top3 substdict;
;;

(*-----------------generation of the type function for a class--- *)

let type_tmpl_gtk_0 = 
  "\
 \n#define  Nsp$(typename)_Private \
 \n#include <nsp/objects.h>\
 \n#include <nsp/gtk/$(typename_dc).h>\
 \n#include <nsp/interf.h>\
 \n#include <nsp/nspthreads.h>"
;;

let type_tmpl_0 = 
  "\
 \n#define  $(typename_nn)_Private \
 \n#include <nsp/objects.h>\
 \n#include <nsp/$(typename_dc).h>\
 \n#include <nsp/interf.h>"
;;

let type_tmpl_1 str = 
  Printf.sprintf 
    "\
 \n\
 \n/* \
 \n * $(typename_nn) inherits from $(parent) \n%s */\
 \n\
 \nint nsp_type_$(typename_dc)_id=0;\
 \nNspType$(typename) *nsp_type_$(typename_dc)=NULL;\
 \n\
 \n/*\
 \n * Type object for $(typename_nn) \
 \n * all the instance of NspType$(typename) share the same id. \
 \n * nsp_type_$(typename_dc): is an instance of NspType$(typename) \
 \n *    used for objects of $(typename_nn) type (i.e built with new_$(typename_dc)) \
 \n * other instances are used for derived classes \
 \n */\
 \nNspType$(typename) *new_type_$(typename_dc)(type_mode mode)\
 \n{\n" str
;;

let type_tmpl_2 str tag = 
  Printf.sprintf 
  "  NspType$(typename) *type= NULL;\
 \n  NspTypeObject *top;\
 \n  if (  nsp_type_$(typename_dc) != 0 && mode == T_BASE )\
 \n    {\
 \n      /* initialization performed and T_BASE requested */\
 \n      return nsp_type_$(typename_dc);\
 \n    }\
 \n  if (( type =  malloc(sizeof(NspType%s))) == NULL) return NULL;\
 \n  type->interface = NULL;\
 \n  type->surtype = (NspTypeBase *) new_type_$(parent_dc)(T_DERIVED);\
 \n  if ( type->surtype == NULL) return NULL;\
 \n  type->attrs = $(typename_dc)_attrs;\
 \n  type->get_attrs = (attrs_func *) $(tp_getattr);\
 \n  type->set_attrs = (attrs_func *) $(tp_setattr);\
 \n  type->methods = $(typename_dc)_get_methods;\
 \n  type->gtk_methods = %s;\
 \n  type->new = (new_func *) new_$(typename_dc);\
 \n\
 \n\
 \n  top = NSP_TYPE_OBJECT(type->surtype);\
 \n  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);\
 \n\n" str tag
;;

let  type_tmpl_4 flag = 
  "  if ( nsp_type_$(typename_dc)_id == 0 ) \n" ^
  "    {\n" ^
  "      /* \n" ^
  "       * the first time we get here we initialize the type id and\n" ^
  "       * an instance of NspType$(typename) called nsp_type_$(typename_dc)\n" ^
  "       */\n" ^
  "      type->id =  nsp_type_$(typename_dc)_id = nsp_new_type_id();\n" ^
  "      nsp_type_$(typename_dc) = type;\n" ^
  "      if ( nsp_register_type(nsp_type_$(typename_dc)) == FALSE) return NULL;\n" ^
  (if flag then 
    "      /* add a ref to nsp_type in the gtype */\n" ^
    "      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_$(typename_dc), $(typecode));\n"
  else
    "") ^
  "      return ( mode == T_BASE ) ? type : new_type_$(typename_dc)(mode);\n" ^
  "    }\n" ^
  "  else \n" ^
  "    {\n" ^
  "      type->id = nsp_type_$(typename_dc)_id;\n" ^
  "      return type;\n" ^
  "    }\n" ^
  "}\n" ^
  "\n" 
;;

let type_tmpl_implements = 
  Printf.sprintf 
  "  /* \
 \n   * $(typename_nn) interfaces can be added here \
 \n   * type->interface = (NspTypeBase *) new_type_b();\
 \n   * type->interface->interface = (NspTypeBase *) new_type_C()\
 \n   * ....\
 \n   */\n" 
;;

let insert_implements typename objinfo substdict = 
  File.write_substitute_pattern type_tmpl_implements substdict;
  File.write_string (build_interfaces objinfo);
  if Overrides.is "override-implements" typename then
    File.write_override "override-implements" typename;
;;


let insert_type typename objinfo is_gtk_class substdict = 
  let implements = 
    if (List.length objinfo.or_implements) <> 0 then 
      let str = List.fold_left
	  (fun x arg -> Printf.sprintf "%s %s" x arg )
	  " * and implements" (List.rev objinfo.or_implements) in 
      str ^ "\n"
    else 
      "" in 
  let tmpl_0 =   if is_gtk_class then type_tmpl_gtk_0 else type_tmpl_0 in
  let str = if is_gtk_class then "$(parent)"  else "$(typename)" in 
  File.write_substitute_pattern tmpl_0  substdict;
  File.write_substitute_pattern (type_tmpl_1 implements)  substdict;
  (* insert declaration for implemented interfaces  *)
  List.iter 
    (fun x -> File.write_string (Printf.sprintf "  NspType%s *t_%s;\n" x (String.lowercase x)))
    (List.rev objinfo.or_implements);
  let tag = if is_gtk_class then "TRUE" else "FALSE" in 

  (* if slot "tp_getattr" is not present then it should be in slotname *)
  if not (Hashtbl.mem substdict "tp_getattr") then 
    (
      let slotname = Printf.sprintf "%s.%s" objinfo.or_name "tp_getattr" in 
      Hashtbl.replace substdict "tp_getattr" (Hashtbl.find substdict slotname)
    );
  if not (Hashtbl.mem substdict "tp_setattr") then 
    (
      let slotname = Printf.sprintf "%s.%s" objinfo.or_name "tp_setattr" in 
      Hashtbl.replace substdict "tp_setattr" (Hashtbl.find substdict slotname)
    );

  File.write_substitute_pattern (type_tmpl_2 str tag) substdict;
  
  if is_gtk_class then 
    insert_methods_gtk typename objinfo substdict 
  else 
    insert_methods typename objinfo substdict ;

  if Overrides.is "override-type" typename then
    File.write_override "override-type" typename
  else
    () ;
  (* insert the implemented interfaces *)
  insert_implements typename objinfo substdict;
  File.write_substitute_pattern (type_tmpl_4 is_gtk_class) substdict;
;;

(*-------------------- init -------------------*) 

let type_tmpl_init str = 
 Printf.sprintf
  "/*\
 \n * initialize $(typename_nn) instances \
 \n * locally and by calling initializer on parent class \
 \n */\
 \n\
 \nstatic int init_$(typename_dc)($(typename_nn) *Obj,NspType$(typename) *type)\
 \n{\
 \n  /* initialize the surtype */ \
 \n  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;\
 \n  Obj->type = type;\
 \n  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;\
 \n  /* specific */\
 \n%s return OK;\
 \n}\
 \n\n" str 
;;

let insert_init _typename objinfo substdict = 
  let pattern =  type_tmpl_init 
    (if check_gtk_class objinfo then 
      ""
    else
      (build_init_fields  objinfo  "Obj") )
  in 
  File.write_substitute_pattern pattern substdict 
;;

(*-------------------- new -------------------*) 

let type_tmpl_new = 
  "/*\
 \n * new instance of $(typename_nn) \
 \n */\
 \n\
 \n$(typename_nn) *new_$(typename_dc)() \
 \n{\
 \n  $(typename_nn) *loc;\
 \n  /* type must exists */\
 \n  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\
 \n  if ( (loc = malloc(sizeof($(typename_nn))))== NULL$(typename_uc)) return loc;\
 \n  /* initialize object */\
 \n  if ( init_$(typename_dc)(loc,nsp_type_$(typename_dc)) == FAIL) return NULL$(typename_uc);\
 \n  return loc;\
 \n}\
 \n\
 \n/*----------------------------------------------\
 \n * Object method redefined for $(typename_nn) \
 \n *-----------------------------------------------*/\n"
;;

let insert_new _typename _objinfo substdict = 
  File.write_substitute_pattern type_tmpl_new substdict 
;;


(*------------------ size--------------------------- *) 

let type_tmpl_size = 
  "/*\
 \n * size \
 \n */\
 \n\
 \nstatic int nsp_$(typename_dc)_size($(typename_nn) *Mat, int flag)\
 \n{\
 \n  return 1;\
 \n}\
 \n\n"
;;

let insert_size typename substdict =
  if Overrides.is "override-size" typename then
    File.write_override "override-size" typename
  else
    File.write_substitute_pattern type_tmpl_size substdict 
;;

(* --------------- type as string------------------- *)

let type_tmpl_type_as_string str =
  Printf.sprintf 
  "/*\
 \n * type as string \
 \n */\
 \n\
 \nstatic char $(typename_dc)_type_name[]=\"$(typename)\";\
 \nstatic char $(typename_dc)_short_type_name[]=\"%s\";\
 \n\
 \nstatic char *nsp_$(typename_dc)_type_as_string(void)\
 \n{\
 \n  return($(typename_dc)_type_name);\
 \n}\
 \n\
 \nstatic char *nsp_$(typename_dc)_type_short_string(NspObject *v)\
 \n{\
 \n  return($(typename_dc)_short_type_name);\
 \n}\
 \n\n" str
;;

let insert_type_as_string _typename is_gtk_class substdict = 
  let str = if is_gtk_class then 
    "$(typename)"
  else
    "$(typename_dc)"
  in 
  File.write_substitute_pattern (type_tmpl_type_as_string str) substdict;
;;

(*--------------- equal and not equal---------------- *)

let type_tmpl_equalities str = 
  Printf.sprintf 
  "/*\
 \n * A == B \
 \n */\
 \n\
 \nstatic int nsp_$(typename_dc)_eq($(typename_nn) *A, NspObject *B)\
 \n{\
 \n  $(typename_nn) *loc = ($(typename_nn) *) B;\
 \n  if ( check_cast(B,nsp_type_$(typename_dc)_id) == FALSE) return FALSE ;\
 \n%s   return TRUE;\
 \n}\
 \n\
 \n/*\
 \n * A != B \
 \n */\
 \n\
 \nstatic int nsp_$(typename_dc)_neq($(typename_nn) *A, NspObject *B)\
 \n{\
 \n  return ( nsp_$(typename_dc)_eq(A,B) == TRUE ) ? FALSE : TRUE;\
 \n}\
 \n\
 \n/*\
 \n * save \
 \n */\
 \n\n" str 
;;

let  insert_equal typename objinfo substdict = 
  if Overrides.is "override-equal" typename then
    File.write_override "override-equal" typename 
  else
    let fields_equal = build_equal_fields  objinfo "H" in 
    let pattern = type_tmpl_equalities fields_equal in 
    File.write_substitute_pattern pattern substdict;
;;

(* ----------------save and load -------------------- *)

let type_tmpl_save_load fs fl str = 
  Printf.sprintf 
    "int nsp_$(typename_dc)_xdr_save(XDR *xdrs, $(typename_nn) *M)\
  \n{\
  \n  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/\
  \n  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ \
  \n  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;\
  \n  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_$(typename_dc))) == FAIL) return FAIL;\
  \n  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;\
  \n%s  return OK;\
  \n}\
  \n\
  \n/*\
  \n * load \
  \n */\
  \n\
  \n$(typename_nn)  *nsp_$(typename_dc)_xdr_load_partial(XDR *xdrs, $(typename_nn) *M)\
  \n{\
  \n%s return M;\
  \n}\
  \n\
  \nstatic $(typename_nn)  *nsp_$(typename_dc)_xdr_load(XDR *xdrs)\
  \n{\
  \n  $(typename_nn) *H = NULL;\
  \n  char name[NAME_MAXL];\
  \n  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL$(typename_uc);\
  \n  if ((H  = nsp_$(typename_dc)_create_void(name,(NspTypeBase *) nsp_type_$(typename_dc)))== NULL$(typename_uc)) return H;\
  \n  if ( nsp_$(typename_dc)_create_partial(H) == FAIL) return NULL$(typename_uc);\
  \n  if ((H  = nsp_$(typename_dc)_xdr_load_partial(xdrs,H))== NULL$(typename_uc)) return H;\
  \n  if ( nsp_$(typename_dc)_check_values(H) == FAIL) return NULL$(typename_uc);\
  \n%s  return H;\
  \n}\
  \n\
  \n" fs fl str 
;;

let insert_save_load typename  objinfo substdict = 
  if Overrides.is "override-save-load" typename then
    File.write_override "override-save-load" typename 
  else
    (
     Hashtbl.replace substdict "ret" "NULL";
     (* get a pattern *) 
     let pattern = get_override_pattern "override-int-create-final" typename in 
     let fields_save = build_save_fields objinfo "M" in 
     let fields_load = build_load_fields objinfo "M" in 
     let pattern = type_tmpl_save_load fields_save fields_load pattern in 
     File.write_substitute_pattern pattern substdict;
    )
;;

(* -----------------------delete------------------ *)

let type_tmpl_delete f1 f2 f3= 
  Printf.sprintf 
  "/*\
 \n * delete \
 \n */\
 \n\
 \nvoid nsp_$(typename_dc)_destroy_partial($(typename_nn) *H)\
 \n{\n%s%s%s}\
 \n\
 \nvoid nsp_$(typename_dc)_destroy($(typename_nn) *H)\
 \n{\
 \n  nsp_object_destroy_name(NSP_OBJECT(H));\
 \n  nsp_$(typename_dc)_destroy_partial(H);\
 \n  FREE(H);\
 \n}\
 \n\n" f1 f2 f3 
;;

let insert_delete typename objinfo substdict = 
  let fields_free1 = build_fields_free1  objinfo "H" in 
  let fields_free2 = build_fields_free2  objinfo "H" in 
  let pattern = get_override_pattern 
      "override-destroy-prelim" typename in
  let pattern = type_tmpl_delete fields_free1 pattern fields_free2 in 
  File.write_substitute_pattern pattern substdict
;;

(* -------------------- info ------------------------ *)

let type_tmpl_info = 
  "/*\
 \n * info \
 \n */\
 \n\
 \nint nsp_$(typename_dc)_info($(typename_nn) *M,int indent,const char *name,int rec_level)\
 \n{\
 \n  const char *pname;\
 \n  if ( M == NULL$(typename_uc)) \
 \n    {\
 \n      Sciprintf(\"Null Pointer $(typename_nn) \\n\");\
 \n      return TRUE;\
 \n    }\
 \n  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\
 \n  Sciprintf1(indent,\"%s\\t=\\t\\t%s\\n\", (pname==NULL) ? \"\" : pname,\
 \n             nsp_$(typename_dc)_type_short_string(NSP_OBJECT(M)));\
 \n  return TRUE;\
 \n}\
 \n\n" 
;;

let insert_info typename substdict =
  if Overrides.is "override-info" typename then
    File.write_override "override-info" typename 
  else
    File.write_substitute_pattern type_tmpl_info  substdict;
;;

(* ------------------------- print ---------------------------- *)

let type_tmpl_print ref_c rec_c_ref str = 
  Printf.sprintf 
  "/*\
 \n * print \
 \n */\
 \n\
 \nint nsp_$(typename_dc)_print($(typename_nn) *M, int indent,const char *name, int rec_level)\
 \n{\
 \n  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\
 \n  if ( M == NULL$(typename_uc)) \
 \n    {\
 \n      Sciprintf(\"Null Pointer $(typename_nn) \\n\");\
 \n      return TRUE;\
 \n    }\
 \n  if (user_pref.pr_as_read_syntax) \
 \n    { \
 \n      Sciprintf1(indent,\"%%s=TO_BE_DONE();\\n\",pname);\
 \n    } \
 \n  else \
 \n    { \
 \n      if ( user_pref.pr_depth  <= rec_level -1 ) \
 \n        {\
 \n          nsp_$(typename_dc)_info(M,indent,pname,rec_level);\
 \n          return TRUE;\
 \n        }\
 \n      Sciprintf1(indent,\"%%s\\t=\\t\\t%%s %s\\n\",pname, \
		     nsp_$(typename_dc)_type_short_string(NSP_OBJECT(M))%s);\
 \n      Sciprintf1(indent+1,\"{\\n\");\
 \n%s    Sciprintf1(indent+1,\"}\\n\");\
 \n    }\
 \n  return TRUE;\
 \n}\n\n"  ref_c rec_c_ref str 
;;

let insert_print typename objinfo substdict =
  if Overrides.is "override-print" typename then
    File.write_override "override-print" typename 
  else
    let (ref_c, rec_c_ref ) = 
      if objinfo.or_byref = false then 
	("", "") 
      else
	("(nref=%d)", ", M->obj->ref_count") in
    let fields_print = build_print_fields  objinfo "M" "print" in 
    let pattern = type_tmpl_print ref_c rec_c_ref fields_print in 
    File.write_substitute_pattern pattern substdict;
;;

(* ------------------ latex ----------------------------------------*) 

let type_tmpl_latex str = 
 Printf.sprintf 
  "/*\
 \n * latex print \
 \n */\n\
 \nint nsp_$(typename_dc)_latex($(typename_nn) *M, int indent,const char *name, int rec_level)\
 \n{\
 \n  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;\
 \n  if ( nsp_from_texmacs() == TRUE ) Sciprintf(\"\\002latex:\\\\[\");\
 \n  Sciprintf1(indent,\"%%s\\t=\\t\\t%%s\\n\",pname, nsp_$(typename_dc)_type_short_string(NSP_OBJECT(M)));\
 \n  Sciprintf1(indent+1,\"{\\n\");\
 \n%s  Sciprintf1(indent+1,\"}\\n\");\
 \n  if ( nsp_from_texmacs() == TRUE ) Sciprintf(\"\\\\]\\005\");\
 \n  return TRUE;\
 \n}\n" str
;;

let insert_latex objinfo substdict = 
  let fields_latex = build_print_fields  objinfo "M" "latex" in 
  let pattern = type_tmpl_latex fields_latex in 
  File.write_substitute_pattern pattern substdict;
;;

(* -------------------interface util --------------------------------------- *) 

let type_tmpl_interface_util str = 
  Printf.sprintf 
  "/*-----------------------------------------------------\
 \n * a set of functions used when writing interfaces \
 \n * for $(typename_nn) objects \
 \n * Note that some of these functions could become MACROS\
 \n *-----------------------------------------------------*/\
 \n\
 \n$(typename_nn)   *nsp_$(typename_dc)_object(NspObject *O)\
 \n{\
 \n  /* Follow pointer */\
 \n  %s\
 \n  /* Check type */\
 \n  if ( $(interface_1) (O,nsp_type_$(typename_dc)_id) $(interface_3) ) return (($(typename_nn) *) O);\
 \n  else \
 \n    Scierror(\"Error:\tArgument should be a %%s\\n\",type_get_name(nsp_type_$(typename_dc)));\
 \n  return NULL;\
 \n}\
 \n\
 \nint Is$(typename)Obj(Stack stack, int i)\
 \n{\
 \n  return $(interface_2)(NthObj(i),nsp_type_$(typename_dc)_id);\
 \n}\
 \n\
 \nint Is$(typename)(NspObject *O)\
 \n{\
 \n  return $(interface_2)(O,nsp_type_$(typename_dc)_id);\
 \n}\
 \n\
 \n$(typename_nn)  *Get$(typename)Copy(Stack stack, int i)\
 \n{\
 \n  if (  Get$(typename)(stack,i) == NULL ) return NULL;\
 \n  return MaybeObjCopy(&NthObj(i));\
 \n}\
 \n\
 \n$(typename_nn)  *Get$(typename)(Stack stack, int i)\
 \n{\
 \n  $(typename_nn) *M;\
 \n  if (( M = nsp_$(typename_dc)_object(NthObj(i))) == NULL$(typename_uc))\
 \n     ArgMessage(stack,i);\
 \n  return M;\
 \n}\
 \n\n" str 
;;

let  insert_interface_util is_gtk_class substdict = 
  let pattern =
    if is_gtk_class then 
      ( type_tmpl_interface_util 
	"HOBJ_GET_OBJECT(O,NULL);") 
    else
      (type_tmpl_interface_util 
	 "if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;" )  in
  File.write_substitute_pattern pattern  substdict;
;;

(* -------------------create --------------------------------------- *) 

let type_tmpl_create_header =
  "/*-----------------------------------------------------\
 \n * constructor \
 \n * if type is non NULL it is a subtype which can be used to \
 \n * create a $(typename_nn) instance \
 \n *-----------------------------------------------------*/\n" 
;;

let type_tmpl_create str1 fields_defval fields_list fields_copy str2 fields_copy_def = 
  Printf.sprintf 
  "\
 \nstatic $(typename_nn) *nsp_$(typename_dc)_create_void(const char *name,NspTypeBase *type)\
 \n{\
 \n $(typename_nn) *H  = (type == NULL) ? new_$(typename_dc)() : type->new();\
 \n if ( H ==  NULL$(typename_uc))\
 \n  {\
 \n   Sciprintf(\"No more memory\\n\");\
 \n   return NULL$(typename_uc);\
 \n  }\
 \n if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULL$(typename_uc);\
 \n NSP_OBJECT(H)->ret_pos = -1 ;\
 \n return H;\
 \n}\n\
 \nint nsp_$(typename_dc)_create_partial($(typename_nn) *H)\
 \n{\
 \n%s  return OK;\
 \n}\n\
 \nint nsp_$(typename_dc)_check_values($(typename_nn) *H)\
 \n{\
 \n%s  return OK;\
 \n}\
 \n\
 \n$(typename_nn) *nsp_$(typename_dc)_create(const char *name,%s,NspTypeBase *type)\
 \n{\
 \n  $(typename_nn) *H  = nsp_$(typename_dc)_create_void(name,type);\
 \n  if ( H ==  NULL$(typename_uc)) return NULL$(typename_uc);\
 \n%s  if ( nsp_$(typename_dc)_check_values(H) == FAIL) return NULL$(typename_uc);\
 \n%s  return H;\
 \n}\
 \n\
 \n\
 \n$(typename_nn) *nsp_$(typename_dc)_create_default(const char *name)\
 \n{\
 \n $(typename_nn) *H  = nsp_$(typename_dc)_create_void(name,NULL);\
 \n if ( H ==  NULL$(typename_uc)) return NULL$(typename_uc);\
 \n%s  if ( nsp_$(typename_dc)_check_values(H) == FAIL) return NULL$(typename_uc);\
 \n return H;\
 \n}\
 \n\n" str1 fields_defval fields_list fields_copy str2 fields_copy_def
;;

let insert_create typename objinfo substdict =
  File.write_substitute_pattern type_tmpl_create_header substdict;
  if Overrides.is "override-create" typename then
    File.write_override "override-create" typename 
  else
    (
     Hashtbl.replace substdict "ret" "NULL";
     let create_partial = build_create_partial objinfo  "H" in 
     let fields_defval = build_defval_fields objinfo "H" in 
     let fields_list = build_list_fields objinfo "" in 
     let fields_copy = build_copy_fields objinfo "H" "" "nsp_object_copy" in 
     let fields_copy_default = build_copy_fields_default objinfo "H" in 
     let pattern = get_override_pattern "override-int-create-final" typename in 
     let pattern = type_tmpl_create create_partial fields_defval fields_list 
	 fields_copy pattern fields_copy_default in 
     File.write_substitute_pattern pattern substdict;
    )
;;

(* ------------------- copy --------------------------------------- *) 

let type_tmpl_copy fields_copy_self copy_partial fields_full_copy_partial_code full_copy_code str = 
  Printf.sprintf 
  "/*\
 \n * copy for gobject derived class  \
 \n */\
 \n\
 \n$(typename_nn) *nsp_$(typename_dc)_copy_partial($(typename_nn) *H,$(typename_nn) *self)\
 \n{\
 \n%s  return H;\
 \n}\n\
 \n$(typename_nn) *nsp_$(typename_dc)_copy($(typename_nn) *self)\
 \n{\
 \n  $(typename_nn) *H  =nsp_$(typename_dc)_create_void(NVOID,(NspTypeBase *) nsp_type_$(typename_dc));\
 \n  if ( H ==  NULL$(typename_uc)) return NULL$(typename_uc);\
 \n%s\
 \n  return H;\
 \n}\
 \n/*\
 \n * full copy for gobject derived class\
 \n */\
 \n\
 \n%s%s%s  return H;\
 \n}\
 \n\
 \n/*-------------------------------------------------------------------\
 \n * wrappers for the $(typename_nn)\
 \n * i.e functions at Nsp level \
 \n *-------------------------------------------------------------------*/\
 \n\n" fields_copy_self copy_partial fields_full_copy_partial_code full_copy_code str
;;

let insert_copy  typename objinfo substdict = 
  Hashtbl.replace substdict "ret" "NULL";
  let fields_copy_self = build_copy_fields objinfo "H" "self" "nsp_object_copy" in 
  let copy_partial = build_copy_partial  objinfo "H" "" in 
  let fields_full_copy_partial_code = 
    build_fields_full_copy_partial_code objinfo substdict "H" "self" "nsp_object_full_copy" in 
  let full_copy_code = build_full_copy_code objinfo substdict "H" in 
  let pattern = get_override_pattern "override-int-create-final" typename in 
  let pattern = type_tmpl_copy
      fields_copy_self copy_partial fields_full_copy_partial_code full_copy_code pattern in 
  File.write_substitute_pattern pattern substdict;
;;

(*------------------- copy for gtk objects ------------------*)


let type_tmpl_copy_gtk = 
  "/*\n"  ^
  " * copy for gobject derived class  \n"  ^
  " */\n"  ^
  "\n"  ^
  "$(typename_nn) *$(typename_dc)_copy($(typename_nn) *self)\n"  ^
  "{\n"  ^
  "  /* return $(parent_dc)_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_$(typename_dc));*/\n"  ^
  "  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_$(typename_dc));\n"  ^
  "}\n"  ^
  "\n"  ^
  "/*-------------------------------------------------------------------\n"  ^
  " * wrappers for the $(typename)\n"  ^
  " * i.e functions at Nsp level \n"  ^
  " *-------------------------------------------------------------------*/\n"
;;

let type_tmpl_copy_gtk_boxed =
  "/*\n"  ^
  " * copy for boxed \n"  ^
  " */\n"  ^
  "\n"  ^
  "$(typename_nn) *$(typename_dc)_copy($(typename_nn) *self)\n"  ^
  "{\n"  ^
  "  return $(parent_dc)_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,\n" ^
  "                              (NspTypeBase *) nsp_type_$(typename_dc));\n"  ^
  "}\n"  ^
  "\n"  ^
  "/*-------------------------------------------------------------------\n"  ^
  " * wrappers for the $(typename)\n"  ^
  " * i.e functions at Nsp level \n"  ^
  " *-------------------------------------------------------------------*/\n"  
;;

let type_tmpl_copy_gtk_pointer =
  "/*\n"  ^
  " * copy for gpointer  \n"  ^
  " */\n"  ^
  "\n"  ^
  "$(typename_nn) *$(typename_dc)_copy($(typename_nn) *self)\n"  ^
  "{\n"  ^
  "  return $(parent_dc)_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed,\n" ^
  "                              (NspTypeBase *) nsp_type_$(typename_dc));\n"  ^
  "}\n"  ^
  "\n"  ^
  "/*-------------------------------------------------------------------\n"  ^
  " * wrappers for the $(typename)\n"  ^
  " * i.e functions at Nsp level \n"  ^
  " *-------------------------------------------------------------------*/\n"  ^
  "\n"  
;;

let insert_copy_gtk  _typename objinfo substdict = 
  let pattern = 
    match objinfo.or_kind with 
    | Pointer -> type_tmpl_copy_gtk_pointer
    | Boxed ->  type_tmpl_copy_gtk_boxed
    | Interface 
    | Object 
    | Struct ->  type_tmpl_copy_gtk in 
  File.write_substitute_pattern pattern substdict;
;;

(*------------------- int_create -----------------------------*) 

let type_tmpl_intcreate str str2 = 
  Printf.sprintf 
  "int int_$(typename_dc)_create(Stack stack, int rhs, int opt, int lhs)\
 \n{\
 \n  $(typename_nn) *H;\
 \n  CheckStdRhs(0,0);\
 \n  /* want to be sure that type $(typename_dc) is initialized */\
 \n  nsp_type_$(typename_dc) = new_type_$(typename_dc)(T_BASE);\
 \n  if(( H = nsp_$(typename_dc)_create_void(NVOID,(NspTypeBase *) nsp_type_$(typename_dc))) == NULL$(typename_uc)) return RET_BUG;\
 \n  /* then we use optional arguments to fill attributes */\
 \n  %s if ( nsp_$(typename_dc)_check_values(H) == FAIL) return RET_BUG;\
 \n  %s  MoveObj(stack,1,(NspObject  *) H);\
 \n  return 1;\
 \n} \
 \n\n" str str2 
;;

let insert_int_create typename objinfo substdict = 
  if Overrides.is "override-intcreate" typename then
    File.write_override "override-intcreate" typename
  else
    (
     Hashtbl.replace substdict "ret" "RET_BUG";
     let pattern = get_override_pattern  "override-int-create-final" typename in 
     let fields_from_attributes=  (build_fields_from_attributes  objinfo "H") in 
     let pattern = type_tmpl_intcreate fields_from_attributes pattern in 
     File.write_substitute_pattern pattern substdict;
    );
;;


(*------------------- import -----------------------------*) 

(* write code for slots and return a new substdict *) 

let write_slots name slot_list  = 
  (* File.write_string "SLOTS: begin\n"; *)

  let write_slot_code slotname = 
    let slot = Printf.sprintf "%s.%s" name slotname in 
    (* if slot[:6] =="tp_as_": slotfunc ="&"  ^ slotfunc *)
    if Overrides.is "override-slot" slot then 
      (
       File.write_override "override-slot" slot;
      ) in 
  List.iter write_slot_code slot_list;
  (* File.write_string "SLOTS: end\n"; *)
;;

let hash_check_slot objinfo  is_gtk_class hash slot =
  let name = objinfo.or_name in 
  if Hashtbl.mem hash slot && Hashtbl.find hash slot <> "0" then
    (
    )
  else
    (
     let slotname = Printf.sprintf "%s.%s" name slot in 
     let name = 
       if is_gtk_class then 
	 (String.lowercase 
	    (Str.global_replace (Str.regexp "_TYPE_") "_" objinfo.or_typecode))
       else
	 name in 
     let slotfunc = Printf.sprintf "_wrap_%s_%s" name slot in 
     if Overrides.is "override-slot" slotname then 
       (
	Hashtbl.replace hash slotname slotfunc;
	File.write_string 
	 (Printf.sprintf "static int %s(Stack stack, int rhs, int opt, int lhs);\n" 
	    slotfunc);
       )
     else 
       (
	if Hashtbl.mem hash (slot ^ "_def") then 
	  (
	   Hashtbl.replace hash slot (Hashtbl.find hash (slot ^ "_def"))
	  )
        else
	  (
	   Hashtbl.replace hash slot "0"
	 )
       )
    )
;;

let get_initial_class_substdict objinfo =
  let substdict = Hashtbl.create 256 in 
  match objinfo.or_kind with 
  | Object 
  | Struct 
  | Pointer 
  | Boxed -> 
      Hashtbl.replace substdict "interface_1" "check_cast";
      Hashtbl.replace substdict "interface_2" "nsp_object_type";      
      Hashtbl.replace substdict "interface_3" " == TRUE ";      
      substdict
  | Interface -> 
      Hashtbl.replace substdict "interface_1" "check_implements";
      Hashtbl.replace substdict "interface_2" "nsp_object_implements";
      Hashtbl.replace substdict "interface_3" " ";      
      substdict
;;

(* write a class *) 

let write_class objinfo failed_tbl = 

  Say.debug (Printf.sprintf "Enter write_class for %s" objinfo.or_name);
  let is_gtk_class = check_gtk_class  objinfo in
  let objinfo = 
    if is_gtk_class then 
      (
      let rep = { objinfo with 
         or_c_name = "Nsp" ^ objinfo.or_c_name;
       } in 
      rep)
    else
      objinfo in 

  File.write_string  ("\n/* -----------" ^ objinfo.or_c_name ^ " ----------- */\n\n" );
  let substdict = get_initial_class_substdict objinfo in 
  Hashtbl.replace substdict "classname"
    ( 
      if Overrides.is "modulename" "_" then 
	let code, _line = (Overrides.get "modulename" "_") in 
	Printf.sprintf "%s.%s" code objinfo.or_name 
      else
	objinfo.or_name
     );

  Hashtbl.replace substdict "typecode"  objinfo.or_typecode;
  Hashtbl.replace substdict "typename_nn"  objinfo.or_c_name;
  Hashtbl.replace substdict "typename"  objinfo.or_name;
  Hashtbl.replace substdict "typename_dc"  (String.lowercase objinfo.or_name);
  Hashtbl.replace substdict "typename_uc"  (String.uppercase objinfo.or_name);

  (* interface can have no parents *) 
  let parent = if is_gtk_class && objinfo.or_parent = "" then 
    "GObject" 
  else objinfo.or_parent in 

  Hashtbl.replace substdict "parent"  parent;
  Hashtbl.replace substdict "parent_dc"  (String.lowercase parent);
  Hashtbl.replace substdict "tp_getattr_def" "int_get_attribute";
  Hashtbl.replace substdict "tp_setattr_def" "int_set_attribute";

  let typename = Hashtbl.find substdict "typename" in
  
  (* check if some slots are overriden and add them in substdict *)
  List.iter 
    (fun slot -> hash_check_slot objinfo  is_gtk_class substdict slot) 
    ["tp_getattr";"tp_setattr"];
  
  (* insert the type defintion  *)
  insert_type typename objinfo is_gtk_class substdict;
  (* insert init function *) 
  insert_init typename objinfo substdict;
  (* insert new function *) 
  insert_new typename objinfo substdict;
  (* check if size was overriden *)
  if is_gtk_class then () 
  else
    insert_size typename  substdict;
  (* type as string *)
  insert_type_as_string typename is_gtk_class substdict;
  (* eq and not equal in type methods  *)
  if is_gtk_class then () 
  else
    insert_equal typename objinfo substdict;
  (* insert override code for save_load  *)
  if is_gtk_class then () 
  else
    insert_save_load typename objinfo substdict;
  (* destroy code  *)
  if is_gtk_class then () 
  else
  insert_delete typename  objinfo substdict;
  (*  info code  *)
  if is_gtk_class then () 
  else
  insert_info typename  substdict;
  (*  print code  *)
  if is_gtk_class then () 
  else
  insert_print typename objinfo substdict;
  (*  latex print  *)
  if is_gtk_class then () 
  else
  insert_latex objinfo substdict;
  (*  set of functions used for writing function wrappers *)
  insert_interface_util is_gtk_class substdict;
  (*  code for create *)
  if is_gtk_class then () 
  else
  insert_create  typename objinfo substdict;
  (*  code for copy  *)
  if is_gtk_class then
    insert_copy_gtk  typename objinfo substdict
  else
    insert_copy  typename objinfo substdict;
  (* # write the int_create inteface  *)
  if is_gtk_class then () 
  else
  insert_int_create typename objinfo substdict;
  (* we write code to the header file here *) 
  Say.debug (Printf.sprintf "Enter write_headers for %s" objinfo.or_name);
  Genheaders.write_header_file objinfo is_gtk_class
    (File.get_override_h_file_name ())
    substdict;
  (* let ( _code1, _code2 ) = write_constructor objinfo in  *)
  (* Hashtbl.replace substdict "tp_init"  code1; *)
  File.write_string "/*-------------------------------------------\n";
  File.write_string " * Methods\n";
  File.write_string " *-------------------------------------------*/\n";
  Say.debug (Printf.sprintf "write constructors for %s" objinfo.or_name);
  let constructors = Genmethods.write_constructors objinfo is_gtk_class failed_tbl in
  Say.debug (Printf.sprintf "write methods for %s" objinfo.or_name);
  Genmethods.write_methods objinfo is_gtk_class;
  Say.debug (Printf.sprintf "write getsets for %s" objinfo.or_name);
  Gengetset.write_getsets objinfo is_gtk_class;
  write_slots objinfo.or_name ["tp_getattr";"tp_setattr"];
  constructors;
;;


let insert_headers ()  = 
  if Overrides.is "headers" "_" then
    File.write_override "headers" "_"
  else
    ()
;;

let insert_enum_value evalue = 
  File.write_string 
    (Printf.sprintf 
       "/*  PyModule_AddIntConstant(module, nspg_constant_strip_prefix(\"%s\", strip_prefix), %s);*/\n"
       evalue.e_tag evalue.e_value);
;;

let insert_enum  enum = 
  Say.debug 
    (Printf.sprintf "enum typecode is %s" enum.e_typecode);
  if enum.e_typecode = "" then 
    (
     File.write_string 
       (Printf.sprintf "/* enum or flags without typecode: %s */\n" enum.e_c_name);
     (* List.iter insert_enum_value enum.e_values *)
    )
  else
    (
     if enum.is_enum then 
       File.write_string 
	 (Printf.sprintf "  nsp_enum_add_constants((NspHash  * )  module, %s, strip_prefix);\n"
	    enum.e_typecode)
     else
       File.write_string 
	 (Printf.sprintf "  nsp_flags_add_constants((NspHash * )module, %s, strip_prefix);\n"
	    enum.e_typecode)
    )
;;

let write_enums parser prefix _fp=
  if List.length parser.enums <> 0 then 
    (
     File.write_string "\n/* ----------- enums and flags ----------- */\n\n";
     File.write_string 
       (Printf.sprintf 
	  "void\n%s_add_constants(NspObject *module, const gchar *strip_prefix)\n{\n"
	  prefix );
     List.iter insert_enum (List.rev parser.enums);
     File.write_string "}\n\n"
    )
  else
    ()
;;

let type_tmpl_copyright = 
    " *\
   \n * This library is free software; you can redistribute it and/or\
   \n * modify it under the terms of the GNU General Public\
   \n * License as published by the Free Software Foundation; either\
   \n * version 2 of the License, or (at your option) any later version.\
   \n *\
   \n * This library is distributed in the hope that it will be useful,\
   \n * but WITHOUT ANY WARRANTY; without even the implied warranty of\
   \n * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\
   \n * General Public License for more details.\
   \n *\
   \n * You should have received a copy of the GNU General Public\
   \n * License along with this library; if not, write to the\
   \n * Free Software Foundation, Inc., 59 Temple Place - Suite 330,\
   \n * Boston, MA 02111-1307, USA.\
   \n */\
   \n"
;;

let write_source fp parser prefix = 
  register_types parser;
  register_types register_parser;
  Say.debug "Enter write source";
  (* TIME Say.warning "--> write_source ";  let t = Sys.time() in *)
  File.set_ppf fp;
  File.write_string "/* -*- Mode: C -*- */\n\n";
  File.write_string "/* This file is generated, please do not edit */\n";
  File.write_string "/* Nsp\n";
  File.write_string (Overrides.get_copyright ());
  File.write_string  type_tmpl_copyright;
  File.write_string "\n\n";

  let objects = (List.rev parser.objects) in 
  let is_gtk_class = 
    if List.length objects <> 0 then 
      check_gtk_class (List.hd objects)
    else
      false in 

  File.write_string "\n\n\n";  (* XXX: a corriger plus tard \n\n is enough *)
  insert_headers ();

  let imports = Overrides.get_imports () in 
  if List.length imports <> 0 then 
    (
     File.write_string "/* ---------- types from other modules ---------- */\n";
     List.iter 
       (fun name -> 
	 File.write_string 
	   (Printf.sprintf "#include <nsp/gtk/%s.h>\n" 
	      (String.lowercase name)))
       (List.rev (Overrides.get_imports ()));
    )
  else
    ( );

  let insert_forward_declaration l = 
    List.iter 
      (fun obj -> 
	if (check_gtk_class obj ) then
	  File.write_string 
	    (Printf.sprintf "#include <nsp/gtk/%s.h>\n" 
	       (String.lowercase obj.or_c_name))
	else
	  File.write_string 
	    (Printf.sprintf "#include <nsp/%s.h>\n" 
	       (String.lowercase obj.or_name))
      ) l in 
  
  if is_gtk_class then 
    (
     File.write_string "/* ---------- forward type declarations ---------- */\n";
     insert_forward_declaration (List.rev parser.boxes);
     insert_forward_declaration (List.rev parser.objects);
     insert_forward_declaration (List.rev parser.interfaces);
     File.write_string "\n";
    )
  else
    ();
      
  Say.debug "Enter write boxed";
  
  let failed_tbl = Hashtbl.create 256 in
  let classes = ((List.rev parser.boxes) @ (List.rev parser.pointers) 
		 @ (List.rev parser.interfaces) @ objects) in 

  (* write classes and keep the list of constructor *)
  
  let constructors = 
    List.fold_left 
      (fun accu obj -> 
	let res = 
	  try 
	    (write_class obj failed_tbl) @ accu
	  with 
	  | _ ->
	      (Say.debug 
		(Printf.sprintf "failed to generate %s" obj.or_c_name);
	       accu) in
	File.write_string "\n";
	res)
      [] classes in
  
  (* here we must insert in failed_tbl the constructors which are 
   * not related to a generated class 
   * useless with the previous code which returns the used constructors 
   *)

  let rec check_in_classes name classes = 
    match classes with 
    | [] -> false 
    | cl :: classes -> 
      begin 
        if cl.or_c_name = name  then 
          true 
        else
          check_in_classes name classes
      end in 

  let check_constructor f = 
     if f.is_constructor_of <> "" then 
       (
        if not (check_in_classes f.is_constructor_of classes) then 
          (
           Hashtbl.replace failed_tbl ((String.lowercase f.is_constructor_of) ^ "_new" ) "_"
          )
       ) in 

  List.iter check_constructor Stringarg.parser.functions;

  (* functions  *)
  Say.debug "Enter write functions";
  Genfunctions.write_functions failed_tbl;
    
  Say.debug "Enter write functions table";
  File.write_string (Genfunctions.write_function_table constructors is_gtk_class failed_tbl);
  (* enums  *)
  Say.debug "Enter write enums";
  write_enums parser prefix fp;
  (*  code added verbatim at the end  *)
  Say.debug "Enter write last";
  File.write_string "\n";
  if Overrides.is "last" "_" then 
    (* File.write_override "last" "_" XXXX we should use this *)
    let code, lineno = Overrides.get "last" "_" in 
    File.setline_override lineno;
    File.write_string code; 
    File.write_string "\n";
    File.resetline();
  else
    File.resetline(); (* XXX should be (); *)
   (* TIME Say.warning (Printf.sprintf "<-- %f sec" ( Sys.time() -. t));*)
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
