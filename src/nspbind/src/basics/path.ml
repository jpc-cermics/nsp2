(***********************************************************************)
(*                                                                     *)
(*                               Interface generator                   *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2015,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 The search path machinery} *)

(** We define a current list of directories to look up for finding files. *)

exception Empty;;

type file_name = string
and dir_name = string
and explicit_file_name = file_name
and file_extension = string
;;

type path = dir_name;;
type t = path;;

external of_explicit_file_name : explicit_file_name -> file_name
  = "%identity"
;;

external of_file_extension : file_extension -> string = "%identity";;
external string_of_extension : file_extension -> string = "%identity";;

let to_file_extension s =
  if s = "" then s else
  match s.[0] with
  | '.' -> s
  | c ->
    invalid_arg (Printf.sprintf
      "Path.to_file_extension: first character of %S is %C; it should be %C."
      s c '.')
;;

let std_io_file_name = "-";;
let builtin_source_file = std_io_file_name;;
let builtin_target_file = std_io_file_name;;

let check_extension fname ext =
  fname = std_io_file_name ||
  Filename.check_suffix (Filename.basename fname) ext
;;

let add_extension fname ext =
  if fname = std_io_file_name then fname else
  fname ^ ext
;;

let get_extension fname =
  try
    let fname = Filename.basename fname in
    let pos = String.rindex fname '.' in
    to_file_extension (String.sub fname pos (String.length fname - pos))
  with
  | Not_found -> to_file_extension ""
;;

let chop_extension fname ext =
  if Filename.check_suffix fname ext
  then Filename.chop_suffix fname ext
  else fname
;;

let change_extension fname ext =
  add_extension (chop_extension fname (get_extension fname)) ext
;;

let check_explicit_extension = check_extension;;
let add_explicit_extension = add_extension;;
let get_explicit_extension = get_extension;;
let chop_explicit_extension = chop_extension;;
let change_explicit_extension = change_extension;;

(* Part I

File names manipulations: path normalization, tilde substitution,
file name directory (and sub directories) creation, etc.

*)
let is_absolute path = not (Filename.is_relative path);;

(** Splits a string according to a character predicate from a given index. *)
let rec split_string s p start =
  let len = String.length s
  and i = ref start in
  while !i < len && p s.[!i] do incr i done;
  if !i >= len then [] else
  begin
    let i0 = !i in
    while !i < len && not (p s.[!i]) do incr i done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s p i1
  end
;;

(* Fixme: should use Filename.dirsep instead of '/' *)
let normalize path =
  let full = is_absolute path in
  let final_slash = path.[String.length path - 1] = '/' in
  let tokens = split_string path (function '/' -> true | _ -> false) 0 in
  let rec remove = function
    | x :: xs ->
      begin match x :: remove xs with
      | x :: [] -> [x]
      | "." :: xs -> xs (* remove "." *)
      | x :: ".." :: xs when x <> ".." -> xs (* remove "dir/.." *)
      | l -> l
      end
    | [] -> [] in
  (* Use Filename.dirsep instead of "/" *)
  let path = String.concat "/" (remove tokens) in
  Printf.sprintf "%s%s%s"
    (if full then "/" else "") path (if final_slash then "/" else "")
;;

let full_path from_dir path =
  (* get full path of a path name (weaker than Junix.realpath) *)
  if is_absolute path then path else normalize (Filename.concat from_dir path)
;;

(* Tilde substitution *)

(* skip to next / *)
let rec next_slash s n =
  if n >= String.length s || s.[n] = '/' then n else
  next_slash s (succ n)
;;

let user_home_dir = try Sys.getenv "HOME" with _ -> "~";;

let tilde_subst fname =
  try
    if fname = "" || fname.[0] <> '~' then fname else
    let len = String.length fname in
    if len = 1 then user_home_dir else
    match fname.[1] with
    | '/' ->
      Filename.concat user_home_dir (String.sub fname 2 (len - 2))
    | _ ->
      let final = next_slash fname 1 in
      let user = String.sub fname 1 (pred final) in
      let pwnam = Unix.getpwnam user in
      if succ final >= len then pwnam.Unix.pw_dir else
       Filename.concat pwnam.Unix.pw_dir
         (String.sub fname (succ final) (len - succ final))
  with
  | Unix.Unix_error (_, _, _) | Sys_error _ | Not_found -> fname
;;

let warning s = prerr_endline (Printf.sprintf "Warning: %s" s);;
let _debug s = prerr_endline (Printf.sprintf "Debug --> %s" s);;
let error s = failwith (Printf.sprintf "Path: %s" s);;

let rec mkdir dir perm =
  (* try to create the directory dir *)
  if not (Sys.file_exists dir) then
    let pdir = Filename.dirname dir in
    (* debug ("Creating directory " ^ pdir ^ "... " ); *)
    mkdir pdir perm;
    Unix.mkdir dir perm;
    (* debug "done" *)
;;

(* cautious_perm_mkdir digs a directory with cautious permissions,
   i.e. drwx------ *)
let cautious_perm_mkdir dir = mkdir dir 0o0700;;

(* Prepare a file access: create its directory and sub-directories if any. *)
let make_base_name fname =
  let dirname = Filename.dirname fname in
  if not (Sys.file_exists dirname) then begin
    try cautious_perm_mkdir dirname with
    | Unix.Unix_error (e, _, _) ->
      (*Say.*)warning (Unix.error_message e)
  end
;;

(* Prepare a file access and clear it before use. *)
let make_pristine_file fname =
  make_base_name fname;
  try
    let oc = open_out_gen [Open_creat; Open_trunc; Open_binary] 0o0600 fname in
    close_out oc
  with
  | Unix.Unix_error (e, _, _) ->
    (*Say.*)warning (Unix.error_message e)
;;

let rec rmdir dir =
  (* debug (Printf.sprintf "rmdir: dir %S" dir); *)
  let dir_handle = Unix.opendir dir in

  let rec loop () =
    try
      let entry = Unix.readdir dir_handle in
      if entry <> "." && entry <> ".." then
      begin
        let entry = Filename.concat dir entry in
        match (Unix.stat entry).Unix.st_kind with
        | Unix.S_DIR -> rmdir entry
        | Unix.S_REG
        | Unix.S_CHR
        | Unix.S_BLK
        | Unix.S_LNK
        | Unix.S_FIFO
        | Unix.S_SOCK ->
          (* debug (Printf.sprintf "rmdir: non directory %S" entry); *)
          Unix.unlink entry
      end;
      loop ()
    with
    | End_of_file -> Unix.closedir dir_handle in

  loop ();
  Unix.rmdir dir
;;

(* The search_path machinery *)
type search_path = path list;;
(** A directory search_path is a list of directory names. *)

let is_readable_directory d =
  try ignore (Sys.readdir d); true with
  | _ -> false
;;

let builtin_search_path = ["."];;

let push, pop, init, get =

  let search_path = ref [] in

  let push d =
   if not (Sys.file_exists d) then
     error (Printf.sprintf "push: %s no such directory" d) else
   if not (Sys.is_directory d) then
     error (Printf.sprintf "push: %s is not a directory" d) else
   if not (is_readable_directory d) then
     error (Printf.sprintf "push: directory %s can not be read" d) else
   search_path := d :: !search_path in

  push,
  (fun () ->
   match !search_path with
   | [] -> raise Empty
   | _ :: rest -> search_path := rest),
  (fun l -> search_path := l),
  (fun () -> !search_path)
;;

let kfind_search_path f search_path s =
  try f s with
  | Sys_error _ as x ->

    let rec do_in = function
      | [] -> raise x
      | p :: pts ->
        let fname = Filename.concat p s in
        (* debug (Print.sprintf "find_search_path %s\n" fname); *)
        try f fname with
        | Sys_error _ -> do_in pts in
    do_in search_path
;;
 (** val kfind_search_path : (explicit_file_name -> 'a) -> t -> file_name -> 'a;;
   [kfind_search_path f search_path s] applies [f] on any file named [s] in
   the search_path, and returns the result of the first call that does not
   fail by raising the exception [Sys_error].
   For instance, [kfind_search_path open_in (Path.get ()) "foo"] searches the
   current search path for a file ["foo"] and returns an input channel to that
   file. *)

let kfind f fname =
  if Filename.is_implicit fname
  then kfind_search_path f (get ()) fname
  else f fname
;;

let remove efname =
  if efname <> std_io_file_name then Sys.remove efname
;;

let path_open_in kfind open_in fname =
  if fname = std_io_file_name then Pervasives.stdin else
  kfind open_in fname
;;

let open_in = path_open_in kfind Pervasives.open_in;;
let open_in_bin = path_open_in kfind Pervasives.open_in_bin;;

let identity x = x;;

let open_explicit_in = path_open_in identity Pervasives.open_in;;
let open_explicit_in_bin = path_open_in identity Pervasives.open_in_bin;;

let path_open_out kfind open_out fname =
  if fname = std_io_file_name then Pervasives.stdout else
  kfind open_out fname
;;

let open_out = path_open_out kfind Pervasives.open_out;;
let open_out_bin = path_open_out kfind Pervasives.open_out_bin;;

let open_explicit_out = path_open_out identity Pervasives.open_out;;
let open_explicit_out_bin = path_open_out identity Pervasives.open_out_bin;;

let close_in ic =
  if ic != stdin then Pervasives.close_in ic
;;

let close_out oc =
  if oc != stdout then Pervasives.close_out oc
;;

let path_with_in_file open_in fname f =
  let ic = open_in fname in
  let r =
    try
      f ic
    with
    | x ->
      close_in ic;
      raise x in
  close_in ic;
  r
;;

let with_in_file fname = path_with_in_file open_in fname;;
let with_in_file_bin fname = path_with_in_file open_in_bin fname;;
let with_explicit_in_file fname = path_with_in_file open_explicit_in fname;;
let with_explicit_in_file_bin fname =
  path_with_in_file open_explicit_in_bin fname
;;

let path_with_out_file open_out fname f =
  let ic = open_out fname in
  let r =
    try
      f ic
    with
    | x ->
      close_out ic;
      remove fname;
      raise x in
  close_out ic;
  r
;;

let with_out_file fname = path_with_out_file open_out fname;;
let with_out_file_bin fname = path_with_out_file open_out_bin fname;;
let with_explicit_out_file fname = path_with_out_file open_explicit_out fname;;
let with_explicit_out_file_bin fname =
  path_with_out_file open_explicit_out_bin fname
;;

let find fname =
  if fname = std_io_file_name then fname else
  let exn_from_file s =
    let error_message = s ^ ": No such file or directory" in
    Sys_error error_message in
  kfind
    (fun s -> if Sys.file_exists s then s else raise (exn_from_file s)) fname
;;

let exists fname =
  fname = std_io_file_name ||
  try kfind
        (fun s -> Sys.file_exists s || raise (Sys_error s)) fname with
  | Sys_error _ -> false
;;

let base_name efname =
  let fname = of_explicit_file_name efname in
  Filename.basename fname
;;

let dir_name efname =
  let fname = of_explicit_file_name efname in
  Filename.dirname fname
;;

let rename efname to_efname = Sys.rename efname to_efname;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
