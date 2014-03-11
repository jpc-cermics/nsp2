(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2013,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 Search path facility} *)

(**
  Defining a search path machinery, i.e. a user definable stack of
  directories where to search for files in the file system.

  File names that has been successfully found into the current search path
  are named 'explicit' file names; they belong to the private type
  [Path.explicit_file_name].

*)

(** {6 File name handling} *)

(** In this module, the special file descriptors corresponding to channels
  [!Pervasives.stdin] and [Pervasives.stdout] are encoded as the file name
  "-".

  Primitives to find files, open or close channels follow this
  convention to properly handle channel [stdin] and [stdout].

  The module also defines and handles file extensions as a private type.
*)

type file_name = string
and dir_name = string
;;
(** The name of a file, the name of a directory. *)

type path = dir_name;;
(** A path is simply a directory name. *)

type t = path;;

val builtin_source_file : file_name;;
val builtin_target_file : file_name;;
(** The names of special files corresponding to builtin channels [stdin] and
  [stdout]. *)

type explicit_file_name = private file_name;;
(** The 'explicit' name of a file is the name of the file as found in the
  current search path.
*)

external of_explicit_file_name : explicit_file_name -> file_name
  = "%identity"
;;
(** The injection from explicit file names to regular file names. *)

(** {6 Operations on explicit file names} *)

val base_name : explicit_file_name -> file_name;;
val dir_name : explicit_file_name -> dir_name;;
(** Equivalent to [!Sys.basename] and [!Sys.dirname] for explicit file
  names. *)

(** {6 File name extensions} *)
type file_extension = private string
;;
(** A file name extension is a string starting by the ['.'] character; it is
  supposed to end the name of a file and classify the file contents.
  To accomodate file names without extensions, a file extension may
  also be empty. *)

val to_file_extension : string -> file_extension;;
(** Projection from string to file extensions.
  [to_file_extension ext_string] checks that [ext_string] could be a valid
  file extension, namely [ext_string] is empty or starts with a ['.']
  character. *)

val of_file_extension : file_extension -> string;;
(** The injection from file extensions to regular strings. *)
val string_of_extension : file_extension -> string;;
(** A synonym for function [of_file_extension]. *)

val get_extension : file_name -> file_extension;;
(** Return the extension of the given file name. *)
val check_extension : file_name -> file_extension -> bool;;
(** Check that the given file name has the given extension.
  File name ["-"] is considered to have any extension. *)

val add_extension : file_name -> file_extension -> file_name;;
(** Add the given extension to the given file name.
  No extension is added to file name ["-"]. *)

val chop_extension :
  file_name -> file_extension -> file_name
;;
(** [chop_extension fname suff] removes extension [suff] from file name [fname].
    Returns [fname] if [fname] is ["-"] or has not the extension [suff]. *)
val change_extension :
  file_name -> file_extension -> file_name
;;
(** The function to change the extension of a file name.
  [change_extension file_name new_ext] adds extension [new_ext] to
  [file_name] without its extension. *)

val get_explicit_extension : explicit_file_name -> file_extension;;
val check_explicit_extension : explicit_file_name -> file_extension -> bool;;
val add_explicit_extension :
  explicit_file_name -> file_extension -> explicit_file_name
;;
val chop_explicit_extension :
  explicit_file_name -> file_extension -> explicit_file_name
;;
val change_explicit_extension :
  explicit_file_name -> file_extension -> explicit_file_name
;;
(** Same as above with explicit file name arguments.
*)

(** {6 The search path machinery} *)

type search_path = path list;;
(** A directory search path is a list of directory names. *)

val builtin_search_path : search_path;;

exception Empty;;
(** This exception is raised when [pop] is called and the current
   directory search path is empty. *)

val get : unit -> search_path;;
val push : dir_name -> unit;;
val pop : unit -> unit;;
val init : search_path -> unit;;
(** The search path machinery:
   define a current list of directories to search files into. *)

val find : file_name -> explicit_file_name;;
(** Return the explicit file name of the first file with the given name
    in the current directory search path.
    Raise [Sys_error] if no such file can be found in the current
    directory search path. *)

val exists : file_name -> bool;;
(** Test if a file with the given name exists in the current directory search path. *)

(** {6 File and channel operations} *)

val remove : explicit_file_name -> unit;;
(** Removes (unlink) the explicit file name argument from the file systeme. *)

val rename : explicit_file_name -> explicit_file_name -> unit;;
(** Rename a file. The first argument is the old name and the
   second is the new name. If there is already another file
   under the new name, [rename] may replace it, or raise an
   exception, depending on your operating system. *)

val open_in : file_name -> in_channel;;
val open_in_bin : file_name -> in_channel;;
val open_out : file_name -> out_channel;;
val open_out_bin : file_name -> out_channel;;
(** Opening files in the current search path. Equivalent to the regular [open_*]
   primitives, if the file name argument is not explicit.
   Raise [Sys_error] if the file cannot be found or cannot be opened. *)

val open_explicit_in : explicit_file_name -> in_channel;;
val open_explicit_in_bin : explicit_file_name -> in_channel;;
val open_explicit_out : explicit_file_name -> out_channel;;
val open_explicit_out_bin : explicit_file_name -> out_channel;;
(** Opening files in the current search path. Equivalent to the regular [open_*]
   primitives, if the file name argument is not implicit.
   Raise [Sys_error] if the file cannot be found or cannot be opened. *)

val close_in : in_channel -> unit;;
val close_out : out_channel -> unit;;

val with_in_file : file_name -> (in_channel -> 'a) -> 'a;;
val with_in_file_bin : file_name -> (in_channel -> 'a) -> 'a;;
val with_out_file : file_name -> (out_channel -> 'a) -> 'a;;
val with_out_file_bin : file_name -> (out_channel -> 'a) -> 'a;;
(** Wrapping functional to securely apply a given I/O function to a file name.
    The channel argument is open with the explicit file name corresponding
    to the file name argument in the current search path.
    In any case, the channel argument is closed before the functional
    application returns.
    For output versions of the [with_] functionals, if execution raises
    an exception before completion, the output file is removed.
*)

val with_explicit_in_file :
  explicit_file_name -> (in_channel -> 'a) -> 'a
;;
val with_explicit_in_file_bin :
  explicit_file_name -> (in_channel -> 'a) -> 'a
;;
val with_explicit_out_file :
  explicit_file_name -> (out_channel -> 'a) -> 'a
;;
val with_explicit_out_file_bin :
  explicit_file_name -> (out_channel -> 'a) -> 'a
;;
(** Same as above for explicit file name arguments. *)

val full_path : dir_name -> file_name -> file_name;;
(** [full_path dir_name path_name] returns the normalized full path name of
  [path_name] which is relative to the directory [dir_name]. *)

val tilde_subst : file_name -> file_name;;
(** Replaces occurrences of "~/" or "~username" to the corresponding
  path names in the file name argument. *)

val mkdir : dir_name -> Unix.file_perm -> unit;;
(** Same as [Unix.mkdir], but also creates parent directories as needed. *)

val make_base_name : file_name -> unit;;
(** [make_base_name file_name] ensures that the base name of [file_name]
  exists in the file system. *)

val make_pristine_file : file_name -> unit;;
(** [make_pristine_file file_name] ensures [file_name] exists and is
  truncated to zero length. *)

val rmdir : dir_name -> unit;;
(** [rmdir dir_name] remove directory [dir_name], deleting all its children.
    Raises [Unix.Unix_error]
    if [dir_name] does not exist or is not a directory. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
