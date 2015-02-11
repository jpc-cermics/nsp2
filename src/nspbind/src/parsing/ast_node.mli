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

(** {6 The polymorphic AST data structure} *)

(** The polymorphic AST node contains the mandatory information for all AST
    nodes: the location of the node, the annotation for the node, the type
    of the node, and a description for the node which is left unspecified as a
    polymorphic type variable at this level of description.
    The polymorphic type variable gives us provision for any kind of
    description type for the nodes: either sum type, record type, or
    abbreviation type;
    we also accept any visibility for the node description type,
    being it either public, private, or abstract. *)
type ('desc, 'info) ast_node = {
   ast_loc : Override_location.t;
   (** The location in the source of the AST node. *)
   ast_desc : 'desc;
   (** The description of the node. *)
   ast_info : 'info;
   (** The support for info. *)
}
;;

type ('desc, 'info) t = ('desc, 'info) ast_node;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
