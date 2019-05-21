open Stringarg;;

let name_search code name new_name = 
  try 
    let _pos = Str.search_forward (Str.regexp_string new_name ) code 0 in 
    new_name
  with 
    Not_found -> name 
;;

(* write code to file for overriden attributes and collect names *)

let write_getset_set_names objinfo  fname gprefix sprefix = 
  let gettername ="int_get_failed" in 
  let settername ="int_set_failed" in 
  let getterobjectname ="int_get_object_failed" in 
  let setterobjectname ="int_set_object_failed" in
  let attrname = objinfo.or_name  ^ "."  ^ fname in 
  let flag = Overrides.is "override-attr" attrname in 

  if flag then
    let code, _lineno = Overrides.get  "override-attr" attrname in 
    File.write_override  "override-attr" attrname ;
    let gettername = name_search code gettername (gprefix  ^ fname) in 
    let settername = name_search code settername (sprefix  ^ fname) in 
    let getterobjectname = name_search code getterobjectname (gprefix  ^ "obj_" ^ fname) in 
    let setterobjectname = name_search code setterobjectname (sprefix  ^ "obj_" ^ fname) in 
    ( gettername, settername, getterobjectname, setterobjectname )
  else
    ( gettername, settername, getterobjectname, setterobjectname )
;;
    
let get_field_accessor_def objinfo fieldname fieldtype = 
  let cast = objinfo.or_c_name in
  let fieldname = 
    if objinfo.or_byref then  "obj->" ^ fieldname else fieldname in 
  if fieldtype <> "" then 
    Printf.sprintf "((%s) ((%s *) self)->%s)" fieldtype cast fieldname
  else
    Printf.sprintf "((%s *) self)->%s"  cast fieldname
;;

let get_field_accessor_boxed objinfo fieldname _fieldtype = 
  Printf.sprintf "NSP_GBOXED_GET(self, %s)->%s" objinfo.or_name  fieldname
;;

let get_field_accessor_gobject objinfo fieldname fieldtype = 
  let castmacro = (Str.global_replace (Str.regexp "_TYPE_") "_" objinfo.or_typecode) in 
  Printf.sprintf "(%s) %s(NSP_GOBJECT_GET(self))->%s" fieldtype castmacro fieldname
;;

let get_field_accessor_pointer  objinfo fieldname _fieldtype = 
  Printf.sprintf "nspg_pointer_get(self, %s)->%s" objinfo.or_c_name  fieldname
;;

let get_field_accessor  objinfo fieldname fieldtype = 
  match objinfo.or_kind with 
  | Pointer -> get_field_accessor_pointer  objinfo fieldname fieldtype
  | Boxed ->  get_field_accessor_boxed  objinfo fieldname fieldtype
  | Interface 
  | Object 
  | Struct ->  
      if objinfo.or_parent = "GObject" || objinfo.or_module = "Gtk" then 
	 get_field_accessor_gobject objinfo fieldname fieldtype
      else
	get_field_accessor_def objinfo fieldname fieldtype
;;


let write_getter objinfo is_gtk_class getterprefix fname ftype x info =
  let funcname = getterprefix  ^ fname in 
  let handler = Stringarg.matcher_get ftype in 
  let info = handler.attr_write_return objinfo false x info in 
  File.write_string 
    (if is_gtk_class then 
      (Printf.sprintf 
	 "static NspObject *%s(NspObject *self,char *attr)\n{\n%s  ret = %s;\n%s\n}\n\n"
	 funcname 
	 (get_varlist info) 
	 (get_field_accessor objinfo fname ftype) 
	 (get_attrcodeafter info))
    else
      (Printf.sprintf 
         "static NspObject *%s(void *self,const char *attr)\n{\n%s  ret = %s;\n%s\n}\n\n"
	 funcname 
	 (get_varlist info) 
	 (get_field_accessor objinfo fname "") 
	 (get_attrcodeafter info)));
  (info, funcname)
;;

let write_getter_obj objinfo getterprefix fname ftype info getterobjectname =
  if info.setobj then 
    (
     let getterobjectname = getterprefix  ^"obj_"  ^ fname in 
     let getterobj_tmpl = 
       (Printf.sprintf
	  "static NspObject *%s(void *self,const char *attr, int *copy)\
	 \n{\n%s  *copy = FALSE;\n  ret = %s;\n%s\n}\n\n"
	  getterobjectname 
	  (get_varlist info) 
	  (get_field_accessor objinfo fname ftype) 
	  (get_attrcodeafter info)) in 
     File.write_string getterobj_tmpl;
     getterobjectname;
    )
  else
    getterobjectname
;;

let write_setter objinfo setterprefix fname ftype x =
  let funcname = setterprefix  ^ fname in 
  let info = Stringarg.fresh_wrapper_info () in 
  let handler = Stringarg.matcher_get ftype in 
  let info = handler.attr_write_set objinfo.or_c_name
      { x with pdflt = None; pnull = false} info objinfo.or_byref in 
  File.write_string 
    (Printf.sprintf 
       "static int %s(void *self,const char *attr, NspObject *O)\n{\n" funcname);
  File.write_string (get_varlist info);
  File.write_string (get_codebefore info);
  File.write_string (get_attrcodebefore info);
  File.write_string "  return OK;\n}\n\n";
  funcname
;;

let write_getset objinfo is_gtk_class getterprefix setterprefix x arg = 
  let  _attrname = objinfo.or_name  ^ "."  ^ x.pname in 
  let (gettername, settername, getterobjectname, setterobjectname ) = 
    write_getset_set_names objinfo x.pname getterprefix  setterprefix in 
  let info = Stringarg.fresh_wrapper_info () in 
  Say.debug (Printf.sprintf "Enter write_getter for %s" x.pname);
  let (info, gettername) = 
    if x.hidden = false && gettername = "int_get_failed" then 
      try
	write_getter objinfo is_gtk_class getterprefix x.pname x.ptype x info
      with _  -> 
	Say.warning (Printf.sprintf "Could not write getter for %s.%s: %s"
		       objinfo.or_name x.pname "exc_info");
	(info, "int_get_failed")
    else
      (info, gettername) in 
  Say.debug (Printf.sprintf "Enter write_getter_obj for %s" x.pname);
  let getterobjectname = 
    if x.hidden = false && getterobjectname = "int_get_object_failed" then 
      try
	write_getter_obj objinfo getterprefix x.pname x.ptype info getterobjectname
      with _ -> 
	failwith 
	  (Printf.sprintf "Could not write getterobj for %s.%s: %s\n"
	     objinfo.or_name x.pname "exc_info")
    else
      getterobjectname in
  Say.debug (Printf.sprintf "Enter write_setter_obj for %s" x.pname);
  let settername = 
    if not is_gtk_class &&  x.hidden = false && settername ="int_set_failed" then 
      try 
	write_setter objinfo setterprefix x.pname x.ptype x 
      with _ ->  
	Printf.printf "Error: Could not write setter for %s.%s: %s\n" 
	  objinfo.or_name x.pname "exc_info";
	setterprefix  ^ x.pname
    else
      settername  in 

  if x.hidden = false && (gettername <> "int_get_failed" || settername <>"int_set_failed") then
    let fixname x = x in 
    let attr_tab = 
      if is_gtk_class then 
	(* XXXX temporary for enabling diff with past code *) 
        let set_obj = 
           if setterobjectname = "int_set_object_failed" then 
              "NULL" 
           else 
              Printf.sprintf "(attr_set_object_function * )%s" setterobjectname in 
	Printf.sprintf 
	  "  { \"%s\", (attr_get_function * )%s, \
          (attr_set_function * )%s, \
	  (attr_get_object_function * )%s, \
          %s },\n" 
          (fixname x.pname) gettername settername getterobjectname set_obj
      else
        Printf.sprintf 
	  "  { \"%s\", (attr_get_function * )%s, \
          (attr_set_function * )%s, \
	  (attr_get_object_function * )%s, \
          (attr_set_object_function * )%s },\n" 
          (fixname x.pname) gettername settername getterobjectname setterobjectname in 
     attr_tab :: arg
  else
     arg
;;

let write_getsets objinfo is_gtk_class  =
  let lower_name = (String.lowercase_ascii objinfo.or_name) in 
  let lower_name1 =
    if is_gtk_class then 
      (String.lowercase_ascii 
	 (Str.global_replace (Str.regexp "_TYPE_") "_" objinfo.or_typecode)) ^ "_" 
    else
      lower_name in 
  let getterprefix =  "_wrap_"  ^ lower_name1  ^ "_get_" in 
  let setterprefix =  "_wrap_"  ^ lower_name1  ^ "_set_" in 

  let attrs_def = 
    (Printf.sprintf 
       "static AttrTab %s_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;\n" 
       lower_name) in 
  
  File.write_string
    "/*-------------------------------------------\
   \n * Attributes\
   \n *-------------------------------------------*/\n\n";

  if List.length objinfo.or_fields = 0 then
    File.write_string attrs_def
  else
    let getsets = 
      List.fold_right 
	(write_getset objinfo is_gtk_class getterprefix setterprefix)
	(List.rev objinfo.or_fields) [] in
    if List.length getsets = 0 then 
      File.write_string attrs_def
    else 
      (
       File.write_string (Printf.sprintf "static AttrTab %s_attrs[] = {\n"  lower_name);
       List.iter 
	 ( fun getset -> File.write_string getset)
	 (List.rev getsets);
       File.write_string "  { NULL,NULL,NULL,NULL,NULL },\n";
       File.write_string "};\n\n";
      )
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
