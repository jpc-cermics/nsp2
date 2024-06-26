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

(* $Id$ *)

open Stringarg;;

let b = Buffer.create 10240;;

let function_tmpl =
  "int _wrap_$(cname_1)(Stack stack, int rhs, int opt, int lhs) /* $(name) */\n" ^
  "{\n" ^
  "$(varlist)" ^
  "$(parseargs)" ^
  "$(codebefore)" ^
  "$(codecallf)" ^
  "$(codeafter)\n" ^
  "}\n\n"
;;

let function_call_tmpl =
  "    $(setreturn)$(cname)($(arglist));\n"
;;

(* check matchers *)

let check_matcher str =
  try
    let _handler = Stringarg.matcher_get str in
    true
  with  _ ->
    false
;;

let rec check_matchers_params params =
  match params with
  | [] -> (true , "")
  | param :: others ->
      if check_matcher param.ptype then
	check_matchers_params others
      else
	(false , param.ptype)
;;

let check_matchers params ret =
  if  check_matcher ret then
    check_matchers_params params
  else
    (false, ret )
;;

(**)
let write_param info param byref =
  let handler = Stringarg.matcher_get param.ptype in
  handler.write_param "" param info byref
;;

let rec write_params info params byref =
  match params with
  | [] -> info
  | param :: rest ->
      write_params (write_param info param byref) rest byref
;;

let get_list_names str =
  let bb = Buffer.create 1024 in
  let rec get_names str n k bb names =
    let rec get_name str n k bb =
      if k < n  then
	match str.[k] with
	| '_' -> k
	| c ->
	    Buffer.add_char bb c;
	    get_name str n (k+1) bb
      else
	k
    in
    Buffer.clear bb;
    if k >= n then
      names
    else
      let pos = get_name str n k bb in
      let name = Buffer.contents bb in
      name :: (get_names str n (pos+1) bb names)
  in
  get_names str (String.length str) 0 bb []
;;

let rec major_minor l major =
  match l with
  | [] -> (major, "0")
  | x :: [] -> (major, x)
  | x :: res -> (major_minor res x )
;;

let availability_tmpl =
  "#else\n" ^
  "int _wrap_$(cname_1)(Stack stack, int rhs, int opt, int lhs) /* $(name) */\n" ^
  "{\n" ^
  "  Scierror(\"Error: function $(cname_1) not available\\n\");\n" ^
  "  return RET_BUG;\n" ^
  "}\n#endif\n"
;;

let deprecated_tmpl =
  "int _wrap_$(cname_1)(Stack stack, int rhs, int opt, int lhs) /* $(name) */\n" ^
  "{\n" ^
  "  Scierror(\"Error: function $(cname_1) is deprecated\\n\");\n" ^
  "  return RET_BUG;\n" ^
  "}\n"
;;

let code_availability f_obj code substdict =
  if f_obj.availability <> "" then
    let l = get_list_names f_obj.availability in
    let tag = (List.hd (List.tl l)) in
    let lib_name = (List.hd l) in
    let prefix = if lib_name = "GLIB" then
      "GLIB" else
      "GTK" in
    let (major,minor) = major_minor l "" in
    if tag = "AVAILABLE" then
      "#if " ^ prefix ^ "_CHECK_VERSION(" ^ major ^ "," ^ minor ^ ",0)\n" ^
      code ^
      File.pattern_to_code_with_buffer availability_tmpl substdict
    else if tag = "DEPRECATED" then
      "#if " ^ prefix ^ "_CHECK_VERSION(" ^  major ^ "," ^ minor ^ ",0)\n" ^
      File.pattern_to_code_with_buffer deprecated_tmpl substdict ^
      "#else\n" ^
      code ^
      "#endif\n"
    else
      code
  else
    code
;;

(**)

let write_function_wrapper or_c_name or_byref f_obj template template_call handle_return is_method substdict =
  let info = {
    optional_args = f_obj.f_options;
    varargs = f_obj.f_varargs;
    varlist= ([], Hashtbl.create 256);
    parsestr= "parse_str";
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
  } in

  (*
  if info.optional_args then
    Printf.printf "function with options %s\n" f_obj.f_c_name;
   *)

  if info.varargs then
    Say.debug
      (Printf.sprintf "function or method with varargs %s" f_obj.f_c_name);

  Hashtbl.replace substdict "setreturn" "";
  Hashtbl.replace substdict "errorreturn" "NULL";

  let info = write_params info f_obj.params or_byref  in

  let info =
    if handle_return then
      (
       (
	match f_obj.ret with
	| Some "none"  -> ();
	| Some _ -> Hashtbl.replace substdict "setreturn" "ret =";
	| _ -> ();
       );
       let owns_return =
	 match f_obj.caller_owns_return with
	 | Some x -> x
	 | None -> false in
       let ret = match f_obj.ret with
       | Some x -> x
       | None -> "none" in
       let handler = Stringarg.matcher_get ret in
       handler.write_return ret owns_return info
      )
    else
      info in

  let deprecated =
    if f_obj.deprecated  then
      Printf.sprintf "  Scierror(\"%%s: deprecated %s\\n\",NspFname(stack)); return RET_BUG;\n"
        f_obj.deprecated_msg
    else
      "" in
  (* if name isn't set, set it to f_obj.name *)
  Hashtbl.replace substdict "name" f_obj.f_name;
  if or_c_name = "" then
    Hashtbl.replace substdict "typename" or_c_name;

  Hashtbl.replace substdict "cname"  f_obj.f_c_name;
  Hashtbl.replace substdict "cname_1"  f_obj.f_c_name;
  Hashtbl.replace substdict "varlist"  (get_varlist info);
  Hashtbl.replace substdict "typecodes"  info.parsestr;
  Hashtbl.replace substdict "parselist"  (get_parselist info);

  let info =
    if is_method then
      (* the list will be reversed and we need a , after self *)
      {info with arglist = info.arglist @ [""];}
    else
      info in
  Hashtbl.replace substdict "arglist"  (get_arglist info);
  Hashtbl.replace substdict "codebefore"
    ( deprecated ^
      (Str.global_replace (Str.regexp_string "return NULL")
	 ( "return" ^ Hashtbl.find substdict "errorreturn")
	 (get_codebefore info)));
  Hashtbl.replace substdict "codeafter"
    (Str.global_replace (Str.regexp_string "return NULL")
       ( "return" ^ Hashtbl.find substdict "errorreturn")
       (get_codeafter info));
  let flags =
    if info.parsestr <> "" then
      (
       if get_parselist info = "" then
	 Hashtbl.replace substdict "parseargs" "  CheckRhs(0,0);\n"
       else
	 (
	  let parse_tmpl = "  if ( GetArgs(stack,rhs,opt,T,$(parselist)) == FAIL) return RET_BUG;\n" in
	  Hashtbl.replace substdict "parseargs"
	    (File.pattern_to_code_with_buffer parse_tmpl  substdict)
	 );
       Hashtbl.replace substdict "extraparams" ", NspObject *args, NspObject *kwargs";
       (* # prepend the keyword list to the variable list *)
       Hashtbl.replace substdict "varlist"
	 (
	  (get_tylist info) ^ (get_kwlist info) ^ (Hashtbl.find substdict "varlist"));
       "METH_?"
      )
    else
      (
       Hashtbl.replace substdict "varlist"
	 ( (get_tylist info)  ^ (Hashtbl.find substdict "varlist"));
       Hashtbl.replace substdict "parseargs" "";
       Hashtbl.replace substdict "extraparams" "";
       "METH_NOARGS"
      ) in

  let code_call = File.pattern_to_code_with_buffer template_call substdict in
  (* is there a special code for calling function ? *)
  if f_obj.calling_code <> "" then
    Hashtbl.replace substdict "codecallf" (f_obj.calling_code ^ "\n")
  else
    Hashtbl.replace substdict "codecallf" code_call;
  let code = File.pattern_to_code_with_buffer template substdict in
  let code = code_availability f_obj code substdict in
  (code, flags)
;;

let get_function_code or_c_name or_byref func =
  if Overrides.is_ignored func.f_c_name then
    (true)
  else
    if Overrides.is "override" func.f_c_name then
      (
       File.write_override "override" func.f_c_name;
       File.write_string "\n\n";
       false
      )
    else
      (
       Say.debug
	 (Printf.sprintf "check function or method %s" func.f_name);
       let handle_return, is_method = (true , false) in
       let substdict =  Hashtbl.create 256 in
       let ret =
	 match func.ret with
	 | Some x -> x
	 | None -> "none" in
       let (test, str) =  check_matchers func.params ret in
       if test then
	 (
	  Say.debug "all matchers for params and returned value were found";
	  try
	    let code, _methflags =
	      write_function_wrapper
		or_c_name or_byref func function_tmpl function_call_tmpl handle_return is_method substdict in
	    File.write_string code;
	    (Say.debug (Printf.sprintf "code for function %s generated" or_c_name));
	    false;
	  with _ ->
	    (Say.debug (Printf.sprintf "Warning: failed to generate function %s" or_c_name);
	     true)
	 )
       else
	 (
	  Say.debug
	    (Printf.sprintf "Warning: failed to generate function %s: matcher missing for %s" func.f_name str);
	  true
	 )
      );
;;

(* write functions code *)

let write_functions failed_tbl =
  let type_tmpl_2 =
    "/*-------------------------------------------\n" ^
    " * functions \n" ^
    " *-------------------------------------------*/\n" in
  File.write_string type_tmpl_2;
  List.iter
    (fun x->
      if not x.is_method && x.is_constructor_of = "" then
	(
	 if get_function_code "" false x then
	   (
	    Hashtbl.replace failed_tbl x.f_c_name "_" ;
	   )
	)
    )
    (List.rev Stringarg.parser.functions);
;;

(* write a function table *)
(*------------------------*)

let buffer_add_function_entries constructions is_gtk_class failed_tbl =
  (* filter methods and ignored functions *)

  let functions =
    List.fold_left
      (fun accu f ->
         if not (f.is_method || Overrides.is_ignored f.f_c_name) then
           f :: accu
         else accu) [] Stringarg.parser.functions in

  let constructions =
    List.fold_left
      (fun accu f ->
         if not (f.is_method || Overrides.is_ignored f.f_c_name) then
           f :: accu
         else accu) [] constructions in

  let buffer_add_construction_entry f =
    let entry =  ((String.lowercase_ascii f.is_constructor_of) ^ "_new" ) in
    let str = f.f_c_name in
    let l = String.length str in
    let new_ended = str.[l-1] = 'w' &&  str.[l-2] = 'e' &&  str.[l-3] = 'n' &&  str.[l-4] = '_' in
    if not (Hashtbl.mem failed_tbl f.f_c_name) then
      (
       Buffer.add_string b
	 (Printf.sprintf "  { \"%s\", _wrap_%s},\n" f.f_c_name f.f_c_name );
       if new_ended then
	 Buffer.add_string b
	   (Printf.sprintf "  { \"%s\", _wrap_%s},\n" entry f.f_c_name ))
    else
      (
       Buffer.add_string b (Printf.sprintf " /* %s %s */\n" entry f.f_c_name )
      ) in

  let buffer_add_function_entry f =
    if not (f.is_method || Overrides.is_ignored f.f_c_name) then
      let entry = (if is_gtk_class then f.f_c_name else f.f_name) in
      let wrapper = f.f_c_name in
      if not (Hashtbl.mem failed_tbl wrapper) then
	Buffer.add_string b
	  (Printf.sprintf "  { \"%s\", _wrap_%s},\n" entry wrapper) in
  (* first constructors *)
  List.iter
    (fun f ->
       if f.is_constructor_of <> "" then
         buffer_add_construction_entry f) constructions;

  (* then functions *)
  List.iter
    (fun f ->
       if f.is_constructor_of = "" then
         buffer_add_function_entry f) functions;
;;

(* the create function is named with the prefix value
 * note that we could have more than one create function
 * up to now we limit the insertion to the first one
 *)

let buffer_add_create_entries prefix =
  let get_create_entry obj =
    let name = String.lowercase_ascii obj.or_name in
    Printf.sprintf "  { \"%s_create\", int_%s_create},\n"
      (String.lowercase_ascii prefix) name  in
  if List.length Stringarg.parser.objects <> 0 then
    List.iter
      (fun x -> Buffer.add_string b (get_create_entry x))
      (* we just generate interface for the first class *)
      [List.hd
	 (List.rev Stringarg.parser.objects)]
;;

let type_tmpl_4 name =
  Printf.sprintf
    "  { NULL, NULL}\
   \n};\
   \n\
   \n/* call ith function in the %s interface */\
   \n\
   \nint %s_Interf(int i, Stack stack, int rhs, int opt, int lhs)\
   \n{\
   \n  return ( *(%s_func[i].fonc))(stack,rhs,opt,lhs);\
   \n}\
   \n\
   \n/* used to walk through the interface table \
   \n    (for adding or removing functions) */\
   \n\
   \nvoid %s_Interf_Info(int i, char **fname, function ( **f))\
   \n{\
   \n  *fname = %s_func[i].name;\
   \n  *f = %s_func[i].fonc;\
   \n}\n" name name name name name name
;;

let type_tmpl_4_gtk name =
  Printf.sprintf
    "  { NULL, NULL}\
   \n};\
   \n\
   \n/* call ith function in the %s interface */\
   \n\
   \nint %s_Interf(int i, Stack stack, int rhs, int opt, int lhs)\
   \n{\
   \n#ifdef NSP_WITH_MAIN_GTK_THREAD\
   \n  return nsp_interface_executed_in_main_thread(i,%s_func[i].fonc,\
   \n  					       &stack,rhs,opt,lhs);\
   \n#else\
   \n  return (*(%s_func[i].fonc))(stack,rhs,opt,lhs);\
   \n#endif\
   \n}\
   \n\
   \n/* used to walk through the interface table \
   \n    (for adding or removing functions) */\
   \n\
   \nvoid %s_Interf_Info(int i, char **fname, function ( **f))\
   \n{\
   \n  *fname = %s_func[i].name;\
   \n  *f = %s_func[i].fonc;\
   \n}\n" name name name name name name name
;;

let ftable_init =
  "/*----------------------------------------------------\
 \n * Interface \
 \n * i.e a set of function which are accessible at nsp level\
 \n *----------------------------------------------------*/\
 \n\n"
;;

let write_function_table constructors is_gtk_class failed_tbl =
  let prefix = Configuration.get_prefix () in
  Buffer.clear b;
  Buffer.add_string b ftable_init;
  Buffer.add_string b (Printf.sprintf "static OpTab %s_func[]={\n" prefix);
  Say.debug "Enter function entries";
  buffer_add_function_entries constructors  is_gtk_class failed_tbl;
  if not is_gtk_class then
    (
     Say.debug "Enter create entries";
     buffer_add_create_entries prefix;
    );

  if is_gtk_class then
    Buffer.add_string b (type_tmpl_4_gtk prefix)
  else
    Buffer.add_string b (type_tmpl_4 prefix);

  Buffer.contents b
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
