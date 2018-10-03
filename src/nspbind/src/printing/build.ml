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

let bb = Buffer.create 10240;;

let build_copy_partial objinfo _varname full =
  let lower_name = String.lowercase objinfo.or_name in 
  let upper_name = String.uppercase objinfo.or_name in 
  Printf.sprintf  
    "  if ( nsp_%s_%scopy_partial(H,self)== NULL) return NULL%s;\n" 
    lower_name full upper_name 
;;

(* returns a call to copy partial of parent class *) 

let build_copy_partial_parent objinfo _varname full =
  let upper_name = String.uppercase objinfo.or_name in 
  let father = objinfo.or_parent in 
  if father <> "Object" then 
    Printf.sprintf "  if ( nsp_%s_%scopy_partial((%s *) H,(%s * ) self ) == NULL) return NULL%s;\n" 
      (String.lowercase father) full ("Nsp" ^father) ("Nsp" ^father) upper_name
  else
    "" 
;;

let build_fields objinfo =
  if objinfo.or_byref then
    Printf.sprintf "nsp_%s *obj;\n"  (String.lowercase objinfo.or_name)
  else
    (
     Buffer.clear bb;
     List.iter 
       (fun x -> 
	 try 
	   Say.debug (Printf.sprintf "Enter build_fields for %s" x.ptype);
	   let handler = Stringarg.matcher_get x.ptype in 
	   Buffer.add_string bb (handler.attr_write_field_declaration objinfo x)
	 with
	 | _ -> Say.debug
	       (Printf.sprintf "Warning: Failed to build field %s" x.ptype)
       )
       objinfo.or_fields;
     Buffer.contents bb;
    )
;;

let build_fields_ref objinfo =
  if objinfo.or_byref = false then 
    (
     ""
    )
  else
    (
     Say.debug (Printf.sprintf "Enter build_fields_ref for %s" objinfo.or_name);
     let cn = (String.lowercase objinfo.or_name) in 
     Buffer.clear bb;
     Buffer.add_string bb
       (Printf.sprintf "typedef struct _nsp_%s nsp_%s;\nstruct _nsp_%s {\n" cn cn cn);
     if List.length  objinfo.or_fields = 0 then 
       Buffer.add_string bb "};\n"
     else
       (
	List.iter
	  ( fun x -> 
	    Say.debug (Printf.sprintf "Enter build_fields_ref for arg of type %s" x.ptype);
	    let handler = Stringarg.matcher_get x.ptype in 
	    Buffer.add_string bb (handler.attr_write_field_declaration objinfo x))
	  objinfo.or_fields;
	Buffer.add_string bb "  int ref_count;\n};\n\n");
     Buffer.contents bb;
    )
;;

(*  used in copy function and in create function  *)
(*  when right_varname =="self" we are in copy function and else  *)
(*  in create function.  *)
(*  if full is"t" then we make a full copy even if we are in *)
(*  "self" mode  *)
(*  f_copy_name is the name of the function used to copy objects *)
(*  i.e nsp_object_copy or nsp_object_full_copy *)

let build_copy_fields objinfo left_varname right_varname f_copy_name = 
  let lower_name = (String.lowercase objinfo.or_name) in 
  let left_varname = if  objinfo.or_byref then left_varname ^ "->obj" else left_varname in 
  Buffer.clear bb;
  let  stop = 
    if objinfo.or_byref then
      (
       if right_varname = "self" then
	 (
	  Buffer.add_string bb "  H->obj = self->obj; self->obj->ref_count++;\n";
	  true
	 )
       else
	 (
	  Buffer.add_string bb 
	    (Printf.sprintf "  if ( nsp_%s_create_partial(H) == FAIL) return NULL%s;\n" 
	       lower_name (String.uppercase lower_name));
	  false
	 )
      )
    else
      (false) in 
  if stop then 
    ()
  else
    (
     List.iter 
       ( fun x -> 
	 Say.debug (Printf.sprintf "Enter build_copy_fields for %s" x.ptype);
	 let handler = Stringarg.matcher_get x.ptype in 
	 Buffer.add_string bb 
           (handler.attr_write_copy objinfo x left_varname right_varname f_copy_name)
	)
       objinfo.or_fields;
    );
  Buffer.contents bb;
;;

(*  used in copy function and in create function  *)
(*  when right_varname =="self" we are in copy function and else  *)
(*  in create function.  *)
(*  if full is"t" then we make a full copy even if we are in *)
(* "self" mode  *)

let build_copy_fields_default objinfo _left_varname = 
  if objinfo.or_byref then
    let lower_name = String.lowercase objinfo.or_name in 
    let upper_name = String.uppercase lower_name in 
    Printf.sprintf "  if ( nsp_%s_create_partial(H) == FAIL) return NULL%s;\n" 
      lower_name upper_name
  else
    ""
;;

(* Note that this function can return a pattern *) 

let build_fields_full_copy_partial_code objinfo _substdic  left_varname right_varname f_copy_name = 
  (*  generate a full_copy_partial function which is useful  *)
  (*  for full_copy of byref objects. *)
  let lower_name = (String.lowercase objinfo.or_name) in 
  let fields_full_copy_self = (build_copy_fields objinfo "H" "self" "nsp_object_full_copy") in 
  let parent_full_copy_partial = build_copy_partial_parent objinfo "unused" "full_" in
  if objinfo.or_byref = false then
    (*  if object is not by reference then full copy is similar to copy  *)
    (*  except that the full_copy object functions should be used *)
    Printf.sprintf 
      "$(typename_nn) *nsp_$(typename_dc)_full_copy_partial($(typename_nn) *H,$(typename_nn) *self)\
      \n{\n%s%s  return H;\n}\n\n"
      parent_full_copy_partial fields_full_copy_self
  else
    (
     let right_varname = right_varname  ^ "->obj" in 
     let left_varname = left_varname  ^ "->obj" in 
     Buffer.clear bb;
     Buffer.add_string bb
       "$(typename_nn) *nsp_$(typename_dc)_full_copy_partial($(typename_nn) *H,$(typename_nn) *self)\n{\n";
     Buffer.add_string bb parent_full_copy_partial;
     Buffer.add_string bb
	    (Printf.sprintf "  if ((H->obj = calloc(1,sizeof(nsp_%s))) == NULL) return NULL%s;\n" 
	       lower_name (String.uppercase lower_name));
     Buffer.add_string bb "  H->obj->ref_count=1;\n";
     List.iter 
	( fun x -> 
	  Say.debug (Printf.sprintf "Enter build_fields_full_copy_partial_code for %s" x.ptype);
	  let handler = Stringarg.matcher_get x.ptype in 
	  Buffer.add_string bb (handler.attr_write_copy objinfo x left_varname right_varname f_copy_name )
        )
        objinfo.or_fields;
     Buffer.add_string bb "  return H;\n}\n\n";
     Buffer.contents bb
    )
;;

let build_save_fields objinfo varname = 
  if List.length objinfo.or_fields = 0 then 
    "" 
  else
    (
     Buffer.clear bb;
     let varname = if objinfo.or_byref then varname  ^ "->obj" else varname in 
     List.iter 
       ( fun x -> 
	 if not x.hidden then 
	   (
	    Say.debug (Printf.sprintf "Enter build_save_fields for %s" x.ptype);
	    let handler = Stringarg.matcher_get x.ptype in 
	    Buffer.add_string bb (handler.attr_write_save varname x objinfo.or_byref )))
       objinfo.or_fields;
     let father = objinfo.or_parent in 
     if father <> "Object" then 
       Buffer.add_string bb 
	 (Printf.sprintf 
	    "  if ( nsp_%s_xdr_save(xdrs, (%s * ) M)== FAIL) return FAIL;\n" 
	    (String.lowercase father) ("Nsp" ^father));
     Buffer.contents bb;
    )
;;

let build_info_fields objinfo varname = 
  if List.length objinfo.or_fields = 0 then 
    ""
  else
    (
     Buffer.clear bb;
     let varname = if objinfo.or_byref then varname  ^"->obj" else varname in 
     List.iter 
       ( fun x -> 
	 if not x.hidden then 
	   (
	    Say.debug (Printf.sprintf "Enter build_info_fields for %s" x.ptype);
	    let handler = Stringarg.matcher_get x.ptype in 
	    Buffer.add_string bb 
              (handler.attr_write_info x.ptype x.pname varname objinfo.or_byref)))
       objinfo.or_fields;
     Buffer.contents bb;
    )
;;

let build_print_fields objinfo varname print_mode = 
  if List.length objinfo.or_fields = 0 then 
    ""
  else
    (
     Buffer.clear bb;
     let father = objinfo.or_parent in
     let varname = if objinfo.or_byref then varname  ^ "->obj" else varname in 
     List.iter 
       ( fun x -> 
	 Say.debug (Printf.sprintf "Enter build_print_fields for %s" x.ptype);
	 let handler = Stringarg.matcher_get x.ptype in 
	 Buffer.add_string bb
	   (handler.attr_write_print objinfo print_mode varname x);
	 if print_mode = "latex" then
	   Buffer.add_string bb "  Sciprintf1(2,\"\\\\\\\\\\n\");\n"
	)
       objinfo.or_fields;
     if father <> "Object" then 
       (
	let arg = if print_mode = "latex" then "FALSE" else "indent+2" in
	Buffer.add_string bb
	  (Printf.sprintf "  nsp_%s_%s((%s * ) M, %s,NULL,rec_level);\n" 
	     (String.lowercase father) print_mode ("Nsp" ^ father) arg));
     Buffer.contents bb;
    )
;;

(* initialize the fields to default values but only for
 * objects that are not byref  
 *)

let build_init_fields objinfo varname = 
  if objinfo.or_byref then 
    Printf.sprintf "  %s->obj = NULL;\n" varname
  else
    (
     Buffer.clear bb;
     List.iter
       ( fun x -> 
	 Say.debug (Printf.sprintf "Enter build_fields_init for %s" x.ptype);
	 try 
	   let handler = Stringarg.matcher_get x.ptype in 
           Buffer.add_string bb (handler.attr_write_init objinfo varname x )
	 with 
	 | _ -> Say.debug
	       (Printf.sprintf "Warning: Failed to initialize field %s" x.ptype)
	)
       objinfo.or_fields;
     Buffer.contents bb;
    )
;;

(* *)

let build_load_fields objinfo varname = 
  let varname = if objinfo.or_byref then varname  ^ "->obj" else varname in 
  let father = objinfo.or_parent in 
  Buffer.clear bb;
  if father <> "Object" then 
    Buffer.add_string bb "  int fid;\n  char name[NAME_MAXL];\n";
  if objinfo.or_byref then 
    Buffer.add_string bb (Printf.sprintf "  %s->ref_count=1;\n" varname);
  List.iter 
    ( fun x -> 
      if not x.hidden then 
	(
	 Say.debug (Printf.sprintf "Enter build_load_fields for %s" x.ptype);
	 let handler = Stringarg.matcher_get x.ptype in 
	 Buffer.add_string bb (handler.attr_write_load varname x  objinfo.or_byref)))
    objinfo.or_fields;
  if List.length objinfo.or_fields <> 0 then
    (
     if father <> "Object" then 
       (
	Buffer.add_string bb "  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;\n";
	Buffer.add_string bb "  if ( fid == nsp_dynamic_id)\n";
	Buffer.add_string bb "    {\n";
	Buffer.add_string bb "     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;\n";
	Buffer.add_string bb "    }\n";
	Buffer.add_string bb "  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;\n";
	Buffer.add_string bb 
	  (Printf.sprintf "  if ( nsp_%s_xdr_load_partial(xdrs,(%s * )M) == NULL) return NULL;\n" 
	     (String.lowercase father) ("Nsp" ^father));
       )
    );
  Buffer.contents bb;
;;

(* *)
     
let build_fields_from_attributes objinfo _varname = 
  let lower_name = (String.lowercase objinfo.or_name) in 
  Buffer.clear bb;
  if objinfo.or_byref then 
    Buffer.add_string bb 
      (Printf.sprintf "  if ( nsp_%s_create_partial(H) == FAIL) return RET_BUG;\n" lower_name)
  else
    ();
   Buffer.add_string bb  
    "  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;\n";
  Buffer.contents bb;
;;

let build_defval_fields objinfo varname = 
  (*  no overrides for the whole function.  If no fields, don't write a func *)
 let father = objinfo.or_parent in 
 let varname = if objinfo.or_byref then varname  ^"->obj" else varname in 
  if List.length objinfo.or_fields = 0 then 
    ("")
  else
    (
     Buffer.clear bb;
     List.iter
       ( fun x -> 
	 Say.debug (Printf.sprintf "Enter build_defval_fields for %s" x.ptype);
	 let handler = Stringarg.matcher_get x.ptype in 
	 Buffer.add_string bb (handler.attr_write_defval objinfo varname x ) 
	)
       objinfo.or_fields;
     if father = "Object" then 
       ()
     else 
       (
	Buffer.add_string bb 
	  (Printf.sprintf "  nsp_%s_check_values((%s * ) H);\n" 
 	     (String.lowercase father) ("Nsp" ^father));
       );
     Buffer.contents bb;
 )
;;
       
let build_fields_free1 objinfo varname = 
  (*  build the code used to free attribute of an object  *)
  Buffer.clear bb;
  let father = objinfo.or_parent in 
  if objinfo.or_byref then
    (
     if father <> "Object" then 
       Buffer.add_string bb 
	 (Printf.sprintf "  nsp_%s_destroy_partial((%s * ) H);\n"  
	    (String.lowercase father) ("Nsp" ^father));
     Buffer.add_string bb 
       (Printf.sprintf "   %s->obj->ref_count--;\n" varname);
     Buffer.add_string bb 
       (Printf.sprintf "  if ( %s->obj->ref_count == 0 )\n   {\n"  varname);
    )
  else
    (
     if father <> "Object" then 
       Buffer.add_string bb 
	 (Printf.sprintf "  nsp_%s_destroy_partial((%s * ) H);\n" 
	    (String.lowercase father) ("Nsp" ^father))
    );
  Buffer.contents bb;  
;;

let build_fields_free2 objinfo varname = 
  let varname = if objinfo.or_byref then varname  ^"->obj" else varname in 
  if List.length objinfo.or_fields = 0 then
    ""
  else
    (
     Buffer.clear bb;
     List.iter 
       ( fun x -> 
	 Say.debug (Printf.sprintf "Enter build_fields_free2 for %s" x.ptype);
	 let handler = Stringarg.matcher_get x.ptype in 
	 Buffer.add_string bb 
	   (handler.attr_free_fields x.ptype x.pname varname objinfo.or_byref))
       objinfo.or_fields;
     if objinfo.or_byref then
       Buffer.add_string bb 
	 (Printf.sprintf "    FREE(%s);\n   }\n" varname);
     Buffer.contents bb;
    ) 
;;

let build_equal_fields objinfo varname = 
  if List.length objinfo.or_fields = 0 then
    ""
  else
    (
     Buffer.clear bb;
     if objinfo.or_byref then 
       Buffer.add_string bb "  if ( A->obj == loc->obj ) return TRUE;\n" ;
     List.iter 
       (fun x -> 
	 try 
	   Say.debug (Printf.sprintf "Enter build_equal_fields for %s" x.ptype);
	   let handler = Stringarg.matcher_get x.ptype in 
	   Buffer.add_string bb 
             (handler.attr_equal_fields objinfo varname x)
	 with 
	 | _ -> 
	     Say.debug
	       (Printf.sprintf "Warning: Failed equal fields for %s" x.ptype)
       )
         
       objinfo.or_fields;
     Buffer.contents bb;
    ) 
;;

(* This function generates the code for full_copy of an object  *)
(* at the end we insert a part which can be inserted from override  *)
(* override_int_create_final  *)

let build_full_copy_code objinfo _substdict _varname = 
  if objinfo.or_byref = false then 
    (*  here full_copy is similar to copy except that full_copy_partial is used *)
    let full_copy_partial =  (build_copy_partial objinfo  "H" "full_") in 
    Printf.sprintf 
      ( 
	"$(typename_nn) *nsp_$(typename_dc)_full_copy($(typename_nn) *self)\
       \n{\
       \n  $(typename_nn) *H  =nsp_$(typename_dc)_create_void(NVOID,(NspTypeBase *) nsp_type_$(typename_dc));\
       \n  if ( H ==  NULL$(typename_uc)) return NULL$(typename_uc);\
       \n%s\n") 
       full_copy_partial
  else
    (
     let lower_name = (String.lowercase objinfo.or_name) in 
     (* let father = objinfo.or_parent in  *)
     Buffer.clear bb;
     Buffer.add_string bb
       "$(typename_nn) *nsp_$(typename_dc)_full_copy($(typename_nn) *self)\n";
     Buffer.add_string bb "{\n";
     Buffer.add_string bb "  $(typename_nn) *H  =nsp_$(typename_dc)_create_void(NVOID,(NspTypeBase *) nsp_type_$(typename_dc));\n";
     Buffer.add_string bb "  if ( H ==  NULL$(typename_uc)) return NULL$(typename_uc);\n";
(*
     if father <> "Object" then
       (
	Buffer.add_string bb
	  (Printf.sprintf "  if ( nsp_%s_full_copy_partial((%s *) H,(%s *) self ) == NULL) return NULL%s;\n" 
	     (String.lowercase father) ("Nsp" ^father) ("Nsp" ^father) (String.uppercase lower_name))
       )
     else
       ();
*)
     Buffer.add_string bb 
       (Printf.sprintf "  if ( nsp_%s_full_copy_partial(H,self)== NULL) return NULL%s;\n" 
	  lower_name (String.uppercase lower_name));

     Buffer.contents bb;
    )
;;

(* used to build the constructir argument list *)

let build_list_fields objinfo _flag = 
  List.fold_right
    ( fun x value -> 
      try 
	Say.debug (Printf.sprintf "Enter build_list_fields for %s" x.ptype);
	let handler = Stringarg.matcher_get x.ptype in 
	( handler.attr_write_create_call objinfo x false) 
	^ (if value = "" then "" else "," ^ value )
      with 
      |_ -> value 

     )
    objinfo.or_fields "" 
;;

(* used when creating a new instance  *)
(* only useful for by ref objects  *)

let build_create_partial objinfo varname = 
  if not objinfo.or_byref then 
    ""
  else
    ( 
      let varname = if objinfo.or_byref then varname  ^ "->obj" else varname in 
      let lower_name = (String.lowercase objinfo.or_name) in 
      let father = objinfo.or_parent in 
      Buffer.clear bb;
      if father <> "Object" then 
	Buffer.add_string bb 
	  (Printf.sprintf "  if ( nsp_%s_create_partial((%s * ) H)== FAIL) return FAIL;\n" 
	     (String.lowercase father) ("Nsp" ^father));
      Buffer.add_string  bb
	(Printf.sprintf "  if((H->obj = calloc(1,sizeof(nsp_%s)))== NULL ) return FAIL;\n" 
	   lower_name);
      Buffer.add_string  bb "  H->obj->ref_count=1;\n";
      List.iter 
	(fun x -> 
	 Say.debug (Printf.sprintf "Enter build_create_partial for %s" x.ptype);
	  let handler = Stringarg.matcher_get x.ptype in 
	  Buffer.add_string bb (handler.attr_write_init objinfo varname x))
	objinfo.or_fields;
      Buffer.contents bb
     )
;;

let build_interfaces objinfo =
  Buffer.clear bb;
  let _str = 
    List.fold_right 
      ( fun interf ti -> 
	let x = (String.lowercase interf) in 
 	Buffer.add_string bb (Printf.sprintf "  t_%s = new_type_%s(T_DERIVED);\n" x x); 
 	Buffer.add_string bb (Printf.sprintf "  %s = (NspTypeBase * ) t_%s;\n" ti x);
	ti ^ "->interface" ) objinfo.or_implements "type->interface" in 
  Buffer.contents bb
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)

