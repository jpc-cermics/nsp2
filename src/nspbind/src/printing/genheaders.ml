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

(* write a header file for a class *)

open Stringarg;;

let get_override_pattern keyword name = 
  if Overrides.is keyword name then 
    let code, lineno = Overrides.get keyword name in 
    let code = 
      Str.global_replace (Str.regexp_string "%(ret)s") "$(ret)" code in 
    Printf.sprintf "$(nl)#line %d \"%s\"$(nl)%s$(line)$(nl)" 
      lineno (File.get_override_h_file_name ()) code 
  else
    "" 
;;

let rec count_newlines s =
  let n = (String.length s) in
  if n = 0 then 0
  else
    let rest = count_newlines (String.sub s 1 (n -1)) in
    if s.[0] == '\n' then
      rest + 1
    else
      rest
;;

let bb = Buffer.create 10240;;

let substitute_pattern filename pattern hash = 
  Buffer.clear bb;
  let line = ref 0 in 
  let pattern =  File.code_with_nl pattern in 
  let subst str = 
    match str with 
    | "nl" -> incr line; "\n"
    | "line" -> 
	Printf.sprintf "#line %d \"%s\"" ( !line + 2 ) filename
    | str -> 
	try 
	  let str = Hashtbl.find hash str in 
	  line := !line + (count_newlines str);
	  str 
	with 
	| _ -> Say.fatal_error 
	      (Printf.sprintf "%s not found in replacement table" str) in 
  Buffer.add_substitute bb subst pattern; 
  Buffer.contents bb;
;;

let build_internal_methods objinfo =
  let code = 
    get_override_pattern "override-internal-methods" objinfo.or_name in 
  if code = "" then 
    get_override_pattern "override-internal-methods" "_"
  else
    code 

let build_internal_methods_protos objinfo = 
  let code = 
    get_override_pattern "override-internal-method-protoss" objinfo.or_name in 
  if code = "" then 
    get_override_pattern "override-internal-methods-protos" "_" 
  else
    code 
;;

let type_header i_start gtk_inc imp im i_public i_private = 
   Printf.sprintf 
  "/* -*- Mode: C -*- */\
 \n#ifndef NSP_INC_$(typename_nn)\
 \n#define NSP_INC_$(typename_nn)\
 \n\
 \n/*\
 \n * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics\
 \n * \
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
 \n */\n\
 \n%s/* $(typename_nn) */\
 \n\
 \n#include <nsp/%s$(parent_dc).h>\
 \n\
 \n/*\
 \n * $(typename_nn) inherits from $(parent)\
 \n */\
 \n\
 \ntypedef struct _$(typename_nn) $(typename_nn) ;\
 \ntypedef struct _NspType$(typename) NspType$(typename) ;\
 \n%s\
 \nstruct _NspType$(typename) {\
 \n  /*< private >*/\
 \n  NSP_TYPE_OBJECT__\
 \n  /*< public >*/\
 \n%s};\
 \n\
 \n$(fields_ref)struct _$(typename_nn) {\
 \n  /*< private >*/\
 \n  Nsp$(parent) father;\
 \n  NspType$(typename)*type;\
 \n  /*< public >*/\
 \n  $(fields)};\
 \n\
 \nextern int nsp_type_$(typename_dc)_id;\
 \nextern NspType$(typename) *nsp_type_$(typename_dc);\
 \n\
 \n/* type instances for $(parent_dc) */\
 \n\
 \nNspType$(typename) *new_type_$(typename_dc)(type_mode mode);\
 \n\
 \n/* instance for $(typename_nn) */\
 \n\
 \n$(typename_nn) *new_$(typename_dc)();\
 \n\
 \n/*\
 \n * Object methods redefined for $(typename_dc) \
 \n */\
 \n\
 \n\
 \n#define NULL$(typename_uc) ($(typename_nn)*) 0\
 \n\
 \nextern $(typename_nn) *nsp_$(typename_dc)_create(const char *name,$(fields_list),NspTypeBase *type);\
 \nextern $(typename_nn) *nsp_$(typename_dc)_create_default(const char *name);\
 \n\
 \n/* from $(typename_nn)Obj.c */\
 \n\
 \nextern $(typename_nn) *nsp_$(typename_dc)_copy($(typename_nn) *H);\
 \nextern void nsp_$(typename_dc)_destroy($(typename_nn) *H);\
 \nextern int nsp_$(typename_dc)_info($(typename_nn) *H, int indent,const char *name, int rec_level);\
 \nextern int nsp_$(typename_dc)_print($(typename_nn) *H, int indent,const char *name, int rec_level);\
 \nextern int nsp_$(typename_dc)_latex($(typename_nn) *H, int indent,const char *name, int rec_level);\
 \nextern $(typename_nn) *nsp_$(typename_dc)_object (NspObject *O);\
 \nextern int Is$(typename)Obj (Stack stack, int i);\
 \nextern int Is$(typename)(NspObject *O);\
 \nextern $(typename_nn) *Get$(typename)Copy (Stack stack, int i);\
 \nextern $(typename_nn) *Get$(typename) (Stack stack, int i);\
 \nextern int nsp_$(typename_dc)_create_partial($(typename_nn) *H);\
 \nextern void nsp_$(typename_dc)_destroy_partial($(typename_nn) *H);\
 \nextern $(typename_nn) * nsp_$(typename_dc)_copy_partial($(typename_nn) *H,$(typename_nn) *self);\
 \nextern $(typename_nn) * nsp_$(typename_dc)_full_copy_partial($(typename_nn) *H,$(typename_nn) *self);\
 \nextern $(typename_nn) * nsp_$(typename_dc)_full_copy($(typename_nn) *self);\
 \nextern int nsp_$(typename_dc)_check_values($(typename_nn) *H);\
 \nextern int int_$(typename_dc)_create(Stack stack, int rhs, int opt, int lhs);\
 \nextern $(typename_nn) *nsp_$(typename_dc)_xdr_load_partial(XDR *xdrs, $(typename_nn) *M);\
 \nextern int nsp_$(typename_dc)_xdr_save(XDR  *xdrs, $(typename_nn) *M);\
 \n\
 \n%s#endif /* NSP_INC_$(typename_nn) */ \n\
 \n#ifdef $(typename_nn)_Private \
 \nstatic int init_$(typename_dc)($(typename_nn) *o,NspType$(typename) *type);\
 \nstatic int nsp_$(typename_dc)_size($(typename_nn) *Mat, int flag);\
 \nstatic char *nsp_$(typename_dc)_type_as_string(void);\
 \nstatic char *nsp_$(typename_dc)_type_short_string(NspObject *v);\
 \nstatic int nsp_$(typename_dc)_eq($(typename_nn) *A, NspObject *B);\
 \nstatic int nsp_$(typename_dc)_neq($(typename_nn) *A, NspObject *B);\
 \nstatic $(typename_nn) *nsp_$(typename_dc)_xdr_load(XDR *xdrs);\
 \nstatic AttrTab $(typename_dc)_attrs[];\
 \nstatic NspMethods *$(typename_dc)_get_methods(void);\
 \n/* static int int_$(typename_dc)_create(Stack stack, int rhs, int opt, int lhs);*/ \
 \nstatic $(typename_nn) *nsp_$(typename_dc)_create_void(const char *name,NspTypeBase *type);\
 \n%s#endif /* $(typename_nn)_Private */\n"
  i_start gtk_inc imp im i_public i_private
;;


let type_header_gtk i_start gtk_inc _imp _im i_public i_private = 
   Printf.sprintf 
  "/* -*- Mode: C -*- */\
 \n#ifndef NSP_INC_$(typename_nn)\
 \n#define NSP_INC_$(typename_nn)\
 \n\
 \n/*\
 \n * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics\
 \n * \
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
 \n */\n\
 \n%s/* $(typename_nn) */\
 \n\
 \n#include <nsp/%s$(parent_dc).h>\
 \n\
 \n/*\
 \n * $(typename_nn) inherits from $(parent)\
 \n * just change some type attributes \
 \n */\
 \n\
 \ntypedef Nsp$(parent) $(typename_nn) ;\
 \ntypedef NspType$(parent) NspType$(typename) ;\
 \n\
 \nextern int nsp_type_$(typename_dc)_id;\
 \nextern NspType$(typename) *nsp_type_$(typename_dc);\
 \n\
 \n/* type instances for $(parent_dc) */\
 \n\
 \nNspType$(typename) *new_type_$(typename_dc)(type_mode mode);\
 \n\
 \n/* instance for $(typename_nn) */\
 \n\
 \n$(typename_nn) *new_$(typename_dc)();\
 \n\
 \n/*\
 \n * Object methods redefined for $(typename_dc) \
 \n */\
 \n\
 \n#define NULL$(typename_uc) ($(typename_nn)*) 0\
 \n\
 \n\
 \n/* from $(typename_nn)Obj.c */\
 \n\
 \nextern $(typename_nn) *nsp_$(typename_dc)_object (NspObject *O);\
 \nextern int Is$(typename)Obj (Stack stack, int i);\
 \nextern int Is$(typename)(NspObject *O);\
 \nextern $(typename_nn) *Get$(typename)Copy (Stack stack, int i);\
 \nextern $(typename_nn) *Get$(typename) (Stack stack, int i);\
 \n\
 \n%s#endif /* NSP_INC_$(typename_nn) */ \n\
 \n#ifdef $(typename_nn)_Private \
 \nstatic int init_$(typename_dc)($(typename_nn) *o,NspType$(typename) *type);\
 \nstatic char *nsp_$(typename_dc)_type_as_string(void);\
 \nstatic char *nsp_$(typename_dc)_type_short_string(NspObject *v);\
 \nstatic AttrTab $(typename_dc)_attrs[];\
 \nstatic NspMethods *$(typename_dc)_get_methods(void);\
 \n/* static int int_$(typename_dc)_create(Stack stack, int rhs, int opt, int lhs);*/ \
 \n%s#endif /* $(typename_nn)_Private */"
  i_start gtk_inc i_public i_private
;;


let get_include_code keyword name override_filename =
  if Overrides.is keyword name then 
    let code, lineno = Overrides.get keyword name in 
    Printf.sprintf "#line %d \"%s\"\n%s$(line)\n" 
      lineno override_filename code 
  else 
    ""
;;

let count_elts hash = 
  let folder _key _value init = init + 1 in 
  Hashtbl.fold folder hash 0
;;

let write_header_file objinfo is_gtk_class override_filename substdict = 
  let name = objinfo.or_name in 
  let filename = "./"  ^ (String.lowercase name)  ^".h" in 
  let parent_is_object = Hashtbl.find substdict "parent_dc" = "object" in
  let is_gtk_class = if parent_is_object then false else is_gtk_class in

  Say.debug (Printf.sprintf "write header file %s" filename);
  Hashtbl.replace substdict "fields" (Build.build_fields objinfo);
  Hashtbl.replace substdict "fields_list" (Build.build_list_fields objinfo "");
  Hashtbl.replace substdict "fields_ref"  (Build.build_fields_ref objinfo );
  Say.debug (Printf.sprintf "Hash table of %d"  (count_elts substdict));
  let func = 
    if is_gtk_class then type_header_gtk else type_header in 
  let pattern =
    func
      (get_include_code "include-start" name override_filename)
      (if is_gtk_class then "gtk/" else "" )
      (build_internal_methods_protos objinfo)
      (build_internal_methods objinfo)
      (get_include_code "include-public" name override_filename)
      (get_include_code "include-private" name override_filename) in 
  let code = substitute_pattern filename pattern substdict  in 
  Path.with_out_file filename 
    (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "%s@." code);
  Say.debug (Printf.sprintf "end write header file %s" filename);
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
