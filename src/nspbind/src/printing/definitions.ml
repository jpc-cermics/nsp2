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

open Lisp_ast;;
open Stringarg;;

(* utilities for sexp *) 

let rec sexp_fold func sexp accu = 
  match sexp with 
  | Cons { car = x ; cdr = sexp } -> 
      sexp_fold func sexp (func x accu)
  | Atom _ | Null -> accu 
;;

let get_atom sexp = 
  match sexp with 
  | Cons { car = Atom name ; cdr = Null;} -> name 
  | Cons _ | Atom _ | Null -> "void"
;;

let get_boolean sexp = 
  match get_atom sexp with 
  | "t" -> true 
  | _ -> false 

(* parameter arguments *)

let get_hidden_status name = 
  if name = "hidden" then true else false
;;

let rec get_parameter_slots sexp int value = 
  match sexp with 
  | Cons { car = Atom name ; cdr = sexp;} ->
      get_parameter_slots sexp (int + 1) 
	(
	 match int with 
	 | 1 -> {value with ptype = name }
	 | 2 -> {value with pname = name }
	 | 3 -> {value with hidden = get_hidden_status name }
	 | 4 -> {value with pdflt = Some name }
	 | 5 -> {value with psize = name }
	 | _ -> value
	)
  | Cons { car = Cons { car = Atom name ; cdr = Cons { car = Atom name_val; cdr = Null}}; cdr=sexp} ->
      if name <> "default" then 
	(
	 Say.warning (Printf.sprintf "ignored keyword %s in parameter list" name);
	 get_parameter_slots sexp (int + 1) value
	)
      else
	get_parameter_slots sexp (int + 1) { value with pdflt = Some name_val; pvarargs = true;}

  | Cons { car = Cons { car = Atom name ; cdr = Null}; cdr=sexp} ->
      if name <> "null-ok" then 
	(
	 Say.warning (Printf.sprintf "ignored keyword %s in parameter list" name);
	 get_parameter_slots sexp (int + 1) value
	)
      else
	get_parameter_slots sexp (int + 1) { value with pnull = true;pvarargs = true;}
  | Cons _ | Atom _ | Null -> value
;;

let get_parameter sexp accu = 
  let parameter  = {
    ptype= "unknown"; (* parameter type *) 
    pname= "";(* parameter name *) 
    pdflt= None; (* used to give a default value *)
    pnull= false; (* unused *)
    psize= ""; (* use to fix sizes for array *)
    hidden = false;
    pvarargs = false;
  } in
  let parameter = (get_parameter_slots sexp 1 parameter) in 
  let parameter = 
    if parameter.pnull = true then 
      match parameter.pdflt with 
      | None -> { parameter with pdflt = Some "NULL";}
      | Some _ -> parameter 
    else
      parameter in
  parameter :: accu
;;

let get_parameters sexp =
  sexp_fold get_parameter sexp []
;;

let rec check_parameters lparams = 
  match lparams with 
  | [] -> false
  | param :: lparams -> 
      if param.pvarargs then true
      else check_parameters lparams
;;

(* when one parameter is with pvarargs true then the followers 
 * must also have pvarargs true *) 

let rec fix_parameters parameters tag = 
  match parameters with 
  | [] -> [] 
  | res :: parameters -> 
      if tag then 
	{res with pvarargs = tag } :: fix_parameters parameters tag
      else
	res :: (fix_parameters parameters res.pvarargs) 
;;

(* define-function or define-method  *)
(*---------------------------------- *)

let function_rec = 
  {
   f_name= "";      (* name of function or method *)
   f_c_name= "";    (* name of c function to call *)
   f_varargs= false ; (* arguments number are variables *)
   params = [] ;    (* parameters of function *)
   ret =  None;    (* type of returned value *)
   caller_owns_return = None ; (* ? *)
   deprecated=false;  (* function is deprecated *)
   deprecated_msg="";  (* function is deprecated *)
   of_object = "";    (* used for methods *)
   is_method = false; (* flag to decide between method and function *)
   is_constructor_of = "";
   in_module = "";  (* XXX *) 
   typecode = ""; 
   f_options = false; (* some arguments are named options *)
 }
;;

let function_rec_set_record name sexp f_rec = 
  match name with 
  | "c-name" -> { f_rec with f_c_name = (get_atom sexp);}
  | "of-object" -> { f_rec with of_object = (get_atom sexp);}
  | "return-type" -> { f_rec with ret = Some (get_atom sexp);}
  | "parameters" -> 
      let parameters = List.rev (get_parameters sexp) in 
      let options = check_parameters parameters in 
      let parameters = 
	if options then 
	  fix_parameters parameters false
	else
	  parameters in 
      { f_rec with params = parameters; f_options = options;}
  | "caller-owns-return" -> 
      { f_rec with caller_owns_return = Some (get_boolean sexp);}
  | "deprecated" -> 
      let value = (get_atom sexp) in 
      { f_rec with deprecated = true; deprecated_msg = value;}
  | "is-constructor-of" -> { f_rec with is_constructor_of = (get_atom sexp);}
  | "in-module" -> { f_rec with in_module = (get_atom sexp);}
  | "gtype-id" -> { f_rec with typecode = (get_atom sexp);}
  | "varargs" -> { f_rec with f_varargs = true;}
  | _ -> 
      Printf.printf "keyword %s is not a function or method parameter\n" name;
      f_rec
;;

let function_args sexp f_rec = 
  sexp_fold 
    (fun sexp f_rec -> 
      match sexp with 
      | Cons { car = Atom name ; cdr = sexp } -> 
	  function_rec_set_record name sexp f_rec
      | Cons _ | Atom _ | Null -> f_rec )
    sexp f_rec 
;;

let select_function sexp = 
  let function_body sexp f_rec = 
    match sexp with 
    | Cons { car = Atom name; cdr = sexp;} -> 
	Some 
	  (function_args sexp { f_rec with f_name = name ;})
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "define-function"; cdr = sexp;} -> 
      function_body sexp { function_rec with is_method = false;}
  | Cons { car = Atom "define-method"; cdr = sexp;} -> 
      function_body sexp { function_rec with is_method = true; }
  | Cons _ | Atom _ | Null -> None
;;


let fix_method f = 
  match f.caller_owns_return, f.ret with
  | None, Some ret  -> 
      let caller_owns_return = 
	if (String.length ret ) >= 6 && (String.sub ret 0 6) = "const-" then
          Some false 
        else
	  ( 
	    if ret = "char*" || ret = "gchar*" || ret = "string" then
	      Some true 
	    else
	      Some false
	   ) in
      { f with caller_owns_return = caller_owns_return;}
  | None, None -> { f with caller_owns_return = Some false;}
  | Some _x , _ -> f
;;

let fix_function f = 
  match f.caller_owns_return, f.ret with
  | None, Some ret  -> 
      let caller_owns_return = 
	if (String.length ret ) >= 6 && (String.sub ret 0 6) = "const-" then
          Some false 
        else
	  (
	   if f.is_constructor_of <> "" then
	     ( Some true )
	   else
             (
	      if ret = "char*" || ret = "gchar*" || ret = "string" then
		Some true 
	      else
		Some false
	     )
	  ) in
      { f with caller_owns_return = caller_owns_return;}
  | None, None -> { f with caller_owns_return = Some false;}
  | Some _x , _ -> f
;;

let fix_function_or_method f = 
  if f.is_method then
    fix_method f
  else
    fix_function f
;;

let rec select_functions lisp_ast accu = 
  match lisp_ast with 
  | [] -> accu 
  |  sexp :: sexps -> 
      match select_function sexp with 
      | Some f -> select_functions sexps ((fix_function_or_method f) :: accu)
      | None -> select_functions sexps accu
;;

(* define-object or define-objectref or define-interface *)
(* define-struct or define-boxed or define-pointer *)
(*------------------------------------------------------ *)

let object_rec = {
  or_name = "";
  or_module = "";
  or_parent = "";
  or_c_name = "";
  or_typecode = "";
  or_byref = false;
  or_kind = Object;
  or_fields = [];
  or_implements = [];
  or_copy_func = "";
  or_release_func = "";
}

let object_rec_set_record name sexp f_rec = 
  match name with 
  | "c-name" -> { f_rec with or_c_name = (get_atom sexp);}
  | "parent" -> { f_rec with or_parent = (get_atom sexp);}
  | "fields" -> { f_rec with or_fields = List.rev (get_parameters sexp);}
  | "implements" -> 
      { f_rec with or_implements = (get_atom sexp ) :: f_rec.or_implements;}
  | "gtype-id" -> { f_rec with or_typecode = (get_atom sexp);}
  | "in-module" -> { f_rec with or_module = (get_atom sexp);}
  | "copy-func" -> { f_rec with or_copy_func = (get_atom sexp);}  
  | "release-func" -> { f_rec with or_release_func = (get_atom sexp);}
  | "vtable" -> f_rec
  | "prerequisite" -> f_rec
  | _ ->
      Printf.printf "keyword %s is not an object parameter\n" name;
      f_rec
;;

let object_args sexp f_rec = 
  sexp_fold 
    (fun sexp f_rec -> 
      match sexp with 
      | Cons { car = Atom name ; cdr = sexp } -> 
	  object_rec_set_record name sexp f_rec
      | Cons _ | Atom _ | Null -> f_rec )
    sexp f_rec 
;;

let select_object sexp = 
  let object_body sexp o_rec = 
    match sexp with 
    | Cons { car = Atom name; cdr = sexp;} -> 
	Some 
	  (object_args sexp { o_rec with or_name = name ;})
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "define-object"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_byref = false;}
  | Cons { car = Atom "define-objectref"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_byref = true; }
  | Cons { car = Atom "define-interface"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_kind = Interface;}
  | Cons { car = Atom "define-struct"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_kind = Struct;}
  | Cons { car = Atom "define-pointer"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_kind = Pointer;}
  | Cons { car = Atom "define-boxed"; cdr = sexp;} -> 
      object_body sexp { object_rec with or_kind = Boxed; or_parent = "GBoxed"; } 
  | Cons _ | Atom _ | Null -> None
;;

let rec select_objects_of_kind kind lisp_ast accu = 
  match lisp_ast with 
  | [] -> accu 
  |  sexp :: sexps -> 
      match select_object sexp with 
      | Some f -> 
	  if f.or_kind = kind then 
	    select_objects_of_kind kind sexps (f :: accu)
	  else
	    select_objects_of_kind kind sexps accu
      | None -> select_objects_of_kind kind sexps accu
;;

let select_objects lisp_ast = 
  select_objects_of_kind Object lisp_ast []
;;

let select_interfaces lisp_ast = 
  select_objects_of_kind Interface lisp_ast []
;;

let select_structures lisp_ast = 
  select_objects_of_kind Struct lisp_ast []
;;

let select_pointers lisp_ast = 
  select_objects_of_kind Pointer lisp_ast []
;;

let select_boxes lisp_ast = 
  select_objects_of_kind Boxed lisp_ast []
;;

(* enum definitions and flags *) 

let enum_rec = {
  e_name = "";
  e_c_name = "";
  e_values = [];
  e_typecode = "";
  e_module = "";
  is_enum = true;
}

let rec get_enum_parameter_slots sexp int value = 
  match sexp with 
  | Cons { car = Atom name ; cdr = sexp;} ->
      get_enum_parameter_slots sexp (int + 1) 
	(
	 match int with 
	 | 1 -> {value with e_tag = name }
	 | 2 -> {value with e_value = name }
	 | _ -> value
	)
  | Cons _ | Atom _ | Null -> value
;;

let get_enum_parameters sexp =
  sexp_fold 
    (fun sexp value -> 
      (get_enum_parameter_slots sexp 1 
	 { e_tag = ""; e_value = "";}) 
      :: value )
    sexp []
;;

let enum_rec_set_record name sexp e_rec = 
  match name with 
  | "c-name" -> { e_rec with e_c_name = (get_atom sexp);}
  | "values" -> { e_rec with e_values = List.rev (get_enum_parameters sexp);}
  | "gtype-id" -> { e_rec with e_typecode = (get_atom sexp);}
  | "in-module" -> { e_rec with e_module = (get_atom sexp);}
  | _ -> failwith (Printf.sprintf "keyword %s is not an enum parameter\n" name);
;;

let enum_args sexp e_rec = 
  sexp_fold 
    (fun sexp e_rec -> 
      match sexp with 
      | Cons { car = Atom name ; cdr = sexp } -> 
	  enum_rec_set_record name sexp e_rec
      | Cons _ | Atom _ | Null -> e_rec )
    sexp e_rec 
;;

let select_enum sexp = 
  let enum_body sexp e_rec = 
    match sexp with 
    | Cons { car = Atom name; cdr = sexp;} -> 
	Some 
	  (enum_args sexp { e_rec with e_name = name ;})
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "define-enum"; cdr = sexp;} -> 
      enum_body sexp { enum_rec with is_enum = true;}
  | Cons { car = Atom "define-flags"; cdr = sexp;} -> 
      enum_body sexp { enum_rec with is_enum = false;}
  | Cons _ | Atom _ | Null -> None
;;

let rec select_enums lisp_ast accu = 
  match lisp_ast with 
  | [] -> accu 
  |  sexp :: sexps -> 
      match select_enum sexp with 
      | Some f -> select_enums sexps (f :: accu)
      | None -> select_enums sexps accu
;;

(* printer for lisp expressions *)

let rec print_sexp sexp = 
  match sexp with 
  | Atom s -> Printf.printf "(atom [%s])" s;
  | Cons c -> 
      Printf.printf "(cons:";
      print_sexp c.car;
      print_sexp c.cdr;
      Printf.printf ")\n";
  | Null -> Printf.printf "null";
;;

let rec print_lisp_ast lisp_ast = 
  match lisp_ast with 
  | [] -> ()
  | sexp :: sexps -> 
      print_sexp sexp;
      Printf.printf "\n";
      print_lisp_ast sexps
;;


(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
