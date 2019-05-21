
type override_keywords = 
  | OVERRIDE_ATTR   
  | OVERRIDE_FIELD_VOID_POINTER_COPY  
  | OVERRIDE_SLOT 
  | HEADERS 
  | COPYRIGHT 
  | OVERRIDE_TYPE 
  | OVERRIDE_IMPLEMENTS  
  | OVERRIDE_SAVE_LOAD   
  | OVERRIDE_SIZE
  | OVERRIDE_TYPE_AS_STRING
  | OVERRIDE_EQUAL   
  | OVERRIDE_CREATE   
  | OVERRIDE_INTCREATE  
  | OVERRIDE_DESTROY_PRELIM 
  | OVERRIDE_PRINT 
  | OVERRIDE_INFO 
  | OVERRIDE_PATH_EXTRACT 
  | OVERRIDE_LOOP 
  | OVERRIDE_INT_CREATE_FINAL 
  | INCLUDE_PUBLIC 
  | INCLUDE_START 
  | INCLUDE_PRIVATE 
  | OVERRIDE_INTERNAL_METHODS 
  | OVERRIDE_INTERNAL_METHODS_PROTOS 
  | INIT 
  | LAST 
  | MODULENAME 
  | IMPORT 
  | IGNORE 
  | OVERRIDE
  | START 
  | IGNORE_GLOB
;;

let of_bindings bindings =
  let l = List.length bindings in
  let t = Hashtbl.create l in
  List.iter (fun (k, v) -> Hashtbl.add t k v) bindings;
  t
;;

let keyword_table =
  of_bindings [
  "override-attr" , OVERRIDE_ATTR   ;
  "override-field-void-pointer-copy" , OVERRIDE_FIELD_VOID_POINTER_COPY  ;
  "override-slot" , OVERRIDE_SLOT ;
  "override-type" , OVERRIDE_TYPE ;
  "override-implements" , OVERRIDE_IMPLEMENTS  ;
  "override-save-load" , OVERRIDE_SAVE_LOAD   ;
  "override-size" , OVERRIDE_SIZE  ;
  "override-type-as-string" , OVERRIDE_TYPE_AS_STRING  ;
  "override-equal" , OVERRIDE_EQUAL   ;
  "override-create" , OVERRIDE_CREATE   ;
  "override-intcreate" , OVERRIDE_INTCREATE  ;
  "override-destroy-prelim" , OVERRIDE_DESTROY_PRELIM ;
  "override-print" , OVERRIDE_PRINT ;
  "override-info" , OVERRIDE_INFO ;
  "override-path-extract" , OVERRIDE_PATH_EXTRACT ;
  "override-loop" , OVERRIDE_LOOP ;
  "override-int-create-final" , OVERRIDE_INT_CREATE_FINAL ;
  "override-internal-methods" , OVERRIDE_INTERNAL_METHODS ;
  "override-internal-methods-protos" , OVERRIDE_INTERNAL_METHODS_PROTOS ;

  "include-public" , INCLUDE_PUBLIC ;
  "include-start" , INCLUDE_START ;
  "include-private" , INCLUDE_PRIVATE ;

  "headers" , HEADERS ;
  "copyright" , COPYRIGHT ;
  "init" , INIT ;
  "last" , LAST ;
  "modulename" , MODULENAME ;
  "import" , IMPORT ;
  "ignore" , IGNORE ;
  "override" , OVERRIDE;
  "start" , START;
  "ignore-glob" , IGNORE_GLOB;
]
;;

type code  = string;;
type line  = int;;
type value = code * line;;
type table = ( string , value ) Hashtbl.t;;
type overrides_keyword_table = ( string , table ) Hashtbl.t;;
let (overrides_keyword_table : overrides_keyword_table ) = Hashtbl.create 256;;
  
(* split a list of names separated by ' ' *)

let rec get_name str n k bb = 
  if k < n  then 
    match str.[k] with 
    | ' ' -> k
    | c -> 
	Buffer.add_char bb c;
	get_name str n (k+1) bb
  else
    k
;;

let rec get_names str n k bb names = 
  Buffer.clear bb;
  if k >= n then 
    names 
  else
    let pos = get_name str n k bb in 
    let name = Buffer.contents bb in 
    name :: (get_names str n (pos+1) bb names)
;;
	   
let get_list_names str = 
  let bb = Buffer.create 1024 in 
  get_names str (String.length str) 0 bb [] 
;;

let store_in_table keyword key value = 
  Say.debug 
    (Printf.sprintf "Insert %s [%s]" keyword key);
  if Hashtbl.mem keyword_table keyword then 
    let hash = 
      if Hashtbl.mem overrides_keyword_table keyword then 
	Hashtbl.find overrides_keyword_table keyword 
      else
	Hashtbl.create 256 in
    Hashtbl.add hash key value;
    Hashtbl.replace overrides_keyword_table keyword hash;
  else
    Say.warning 
      (Printf.sprintf "%s is not recognized as a possible override keyword" keyword);
;;

(* for imports we need to keep track of order *)

type import_list = {
    mutable imports : string list;
  }
;;

let import_list = 
  {
   imports = [];
  }
;;

let rec store_import keyword l = 
  match l with 
  | "import" :: (_a :: ( _b :: (c :: l))) -> 
      store_in_table keyword c ("_",0);
      import_list.imports <- c:: import_list.imports;
      store_import keyword l;
  | _ -> ()
;;

let get_imports () = import_list.imports
;;
  
(*
let get_imports () = import_list 
  let keyword = "import" in 
  if Hashtbl.mem keyword_table keyword then 
    if Hashtbl.mem overrides_keyword_table keyword then 
      let hash = Hashtbl.find overrides_keyword_table keyword in 
      (* we use the list to keep order *)
      (* Hashtbl.fold (fun k _v accu -> k :: accu) hash [] *)
      List.fold_right (fun x arg -> !x :: arg )
    else
      []
  else
    []
;;
*)

let store keyword key value = 
  match keyword with 
  | "ignore" 
  | "ignore-glob"  -> 
      let code, _line = value in 
      Say.debug 
	(Printf.sprintf "Insert-split %s [%s]" keyword code);
      let l = get_list_names code in 
      List.iter 
	(fun x -> store_in_table keyword x ("_",0)) l
  | "import" -> 
      let code, _line = value in 
      Say.debug 
	(Printf.sprintf "Insert-split %s [%s]" keyword code);
      let l = "import" :: (get_list_names code) in 
      store_import keyword l 
  | _  -> store_in_table keyword key value
;;

let store_ignore _names = ()
;;

let is keyword key = 
  if Hashtbl.mem keyword_table keyword then 
    if Hashtbl.mem overrides_keyword_table keyword then 
      let hash = Hashtbl.find overrides_keyword_table keyword in 
      Hashtbl.mem hash key
    else
      false
  else
    false
;;

let get keyword key = 
  if Hashtbl.mem keyword_table keyword then 
    if Hashtbl.mem overrides_keyword_table keyword then 
      let hash = Hashtbl.find overrides_keyword_table keyword in 
      Hashtbl.find hash key
    else
      failwith (Printf.sprintf "%s: not recognized as a possible key in %s" 
		  key keyword)
  else
    failwith (Printf.sprintf "%s: not recognized as a possible override keyword" 
		keyword)
;;

let get_copyright () =
  if is "copyright" "_" then 
    let (code, _line ) = get "copyright" "_"  in 
    code
  else
    " * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics\n"
;;

(* ignore keyword *) 

let match_glob pattern str = 
  let regexp = 
    Str.global_replace (Str.regexp "\\*") "[A-Za-z0-9_]*" pattern in 
  let test = Str.replace_first (Str.regexp regexp) "" str in 
  if test = "" then 
    true 
  else
    false
;;

let rec is_ignore_glob_list l key = 
  match l with 
  | [] -> false 
  | p :: l -> 
      if match_glob p key then 
	true
	  else
	is_ignore_glob_list l key
;;

let is_ignored_glob key =
  let keyword = "ignore-glob" in 
  if Hashtbl.mem keyword_table keyword then 
    if Hashtbl.mem overrides_keyword_table keyword then 
      let hash = Hashtbl.find overrides_keyword_table keyword in 
      let l = Hashtbl.fold (fun k _v accu -> k :: accu) hash [] in 
      is_ignore_glob_list l key 
    else
      false
  else
    false
;;

let is_ignored_std key = 
  let keyword = "ignore" in 
  if Hashtbl.mem keyword_table keyword then 
    if Hashtbl.mem overrides_keyword_table keyword then 
      let hash = Hashtbl.find overrides_keyword_table keyword in 
      if Hashtbl.mem hash key then 
	true 
      else
	false
    else
      false
  else
    false 
;;
      
let is_ignored key = 
  if is_ignored_std key then 
    true
  else
    is_ignored_glob key
;;
    

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
