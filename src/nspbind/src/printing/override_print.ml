(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2014,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Mtlb_ast;;
open Format;;
open Stringarg;;

(* pretty printer of an ast to a target language *)

let print_override ppf override =
  match override.Ast_node.ast_desc with
  | Ignore(_names) -> fprintf ppf "---------------[ignore]-------------\n" 
  | Override2(s,code) -> fprintf ppf "---------------[%s]-------------\n%s" s code
  | Override3(s,s2,code) -> fprintf ppf "---------------[%s][%s]------------\n%s" s s2 code
  | Override4(s,s2,s3,code) -> fprintf ppf "---------------[%s][%s][%s]-------------\n%s" s s2 s3 code
;;

let rec print_override_list ppf overrides = 
  match overrides with
  | [] -> ()
  | elt :: elts ->
      fprintf ppf "@[<hov>%a@]" print_override elt;
      List.iter
        (function arg ->
          fprintf ppf "@,@[<hov>%a@]"
            print_override arg) elts

and print_ast_desc ppf = function
    Ast(exprs) -> 
      fprintf ppf "@[<v>%a@]" print_override_list exprs
;;

(* insert an override ast into overrides data  *)

let insert_override override =
  let line = Mtlb_location.get_line override.Ast_node.ast_loc in 
  match override.Ast_node.ast_desc with
  | Ignore(names) -> Overrides.store_ignore names;
  | Override2(s,code) ->  Overrides.store s "_" ( code, line );
  | Override3(s,s2,code) -> Overrides.store s s2 (code, line);
  | Override4(s,s2,s3,code) -> Overrides.store s (s2 ^"." ^ s3) (code, line);
;;

let rec insert_override_list overrides = 
  match overrides with
  | [] -> ()
  | elt :: elts ->
      insert_override elt;
      insert_override_list elts;

and insert_ast = function
    Ast(exprs) ->  insert_override_list exprs
;;
    
(* Read an overrides file and fill overrides data *)

let overrides_init efname = 
  Say.debug "read_mtlb_file";
  let m_ast = Mtlb_to_ast.read_mtlb_file efname in
  Say.debug "insert_ast";
  insert_ast m_ast.Ast_node.ast_desc;
;;

let fill_parser parser lisp_ast = 
  let functions = Definitions.select_functions lisp_ast [] in 
  parser.functions <- functions;
  let objects = Definitions.select_objects lisp_ast in 
  parser.objects <- objects;
  let interfaces = Definitions.select_interfaces lisp_ast in 
  parser.interfaces <- interfaces;
  let structures = Definitions.select_structures lisp_ast in 
  parser.structures <- structures;
  let pointers = Definitions.select_pointers lisp_ast in 
  parser.pointers <- pointers;
  let boxes = Definitions.select_boxes lisp_ast in 
  parser.boxes <- boxes;
  let enums = Definitions.select_enums lisp_ast [] in 
  parser.enums <- enums;
;;

let set_prefix_from_object objects = 
  if List.length objects = 1 then 
    let obj = List.hd objects in 
    Configuration.set_prefix_from_object obj.or_name
;;

let definitions_init efname = 
  let (lisp_ast, register_ast) = Lisp_to_ast.read_lisp_file efname in
  (* Definitions.print_lisp_ast lisp_ast; *)
  fill_parser Stringarg.parser lisp_ast;
  fill_parser Stringarg.register_parser register_ast;
  set_prefix_from_object parser.objects
;;

(*
  let functions = Definitions.select_functions lisp_ast [] in 
  Stringarg.parser.Stringarg.functions <- functions;
  let objects = Definitions.select_objects lisp_ast in 
  Stringarg.parser.Stringarg.objects <- objects;
  let interfaces = Definitions.select_interfaces lisp_ast in 
  Stringarg.parser.Stringarg.interfaces <- interfaces;
  let structures = Definitions.select_structures lisp_ast in 
  Stringarg.parser.Stringarg.structures <- structures;
  let pointers = Definitions.select_pointers lisp_ast in 
  Stringarg.parser.Stringarg.pointers <- pointers;
  let boxes = Definitions.select_boxes lisp_ast in 
  Stringarg.parser.Stringarg.boxes <- boxes;
;;
*)

let translate_file ppf dst_efname =
  File.set_filenames dst_efname 
    (Configuration.get_path_to_override_for_c () 
     ^ (Configuration.get_overrides_source_file ()))
    (Configuration.get_path_to_override_for_h () 
     ^ (Configuration.get_overrides_source_file ()));
  Codegen.write_source ppf Stringarg.parser 
      (Configuration.get_prefix ())
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
