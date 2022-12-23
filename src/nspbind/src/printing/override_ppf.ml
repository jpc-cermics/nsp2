(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format;;

(* BEGIN: From the 4.01 Format module *)
(*
type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
}
;;

let pp_set_formatter_out_functions ppf out_funs =
  match out_funs with
  | {
      out_string = pp_out_string;
      out_flush = pp_out_flush;
      out_newline = pp_out_newline;
      out_spaces = pp_out_spaces;
    } ->
    Format.pp_set_all_formatter_output_functions ppf
      ~out:pp_out_string ~flush:pp_out_flush
     ~newline:pp_out_newline ~spaces:pp_out_spaces
;;

let pp_get_formatter_out_functions ppf () =
  let (pp_out_string, pp_out_flush,
       pp_out_newline, pp_out_spaces) =
    Format.pp_get_all_formatter_output_functions ppf () in
  {
    out_string = pp_out_string;
    out_flush = pp_out_flush;
    out_newline = pp_out_newline;
    out_spaces = pp_out_spaces;
  }
;;
*)
(* END: From the 4.01 Format module *)

type tag_new_line_markers = {
  m_tag_new_line : string;
  n_tag_new_line : string;
}
;;

type tag_handler = {
  tag_funs : formatter_stag_functions;
  out_funs : formatter_out_functions;
  mark_tags : bool;
}
;;

let set_tag_handler ppf h =
(*  prerr_endline "Setting the tag handler to a ppf argument";*)
  pp_set_formatter_stag_functions ppf h.tag_funs;
  pp_set_formatter_out_functions ppf h.out_funs;
  pp_set_mark_tags ppf h.mark_tags;
;;

let get_tag_handler ppf =
(*  prerr_endline "Getting the tag handler of a ppf argument";*)
 {
   tag_funs = pp_get_formatter_stag_functions ppf ();
   out_funs = pp_get_formatter_out_functions ppf ();
   mark_tags = pp_get_mark_tags ppf ();
 }
;;

let m_tag_tag_handlers = ref [];;
let push_tag_handler h =
(*  prerr_endline "Adding a tag handler to the stack";*)
  m_tag_tag_handlers := h :: !m_tag_tag_handlers
and top_tag_handler () =
(*  prerr_endline "Getting top tag handler";*)
  match !m_tag_tag_handlers with
  | [] -> failwith "top_tag_handler"
  | h :: _ -> h
and pop_tag_handler () =
  match !m_tag_tag_handlers with
  | [] -> failwith "pop_tag_handler"
  | _ :: hs ->
(*    prerr_endline "Removing a tag handler from the stack";*)
    m_tag_tag_handlers := hs
;;

let make_tag_ppf tnlm ppf =

  let ppf_tag_handler = get_tag_handler ppf in

  let m_tag_out_newline () =
    ppf_tag_handler.out_funs.out_string
      tnlm.m_tag_new_line 0 (String.length tnlm.m_tag_new_line) in

  let n_tag_out_newline () =
    ppf_tag_handler.out_funs.out_string
      tnlm.n_tag_new_line 0 (String.length tnlm.m_tag_new_line) in

  let rec m_tag_mark_open_tag = function
    | Format.String_tag "m" | Format.String_tag "" ->
      push_tag_handler (get_tag_handler ppf);
      set_tag_handler ppf m_tag_tag_handler;
      (*      "(.m_tag." *)
      ""
    | Format.String_tag "n" ->
      push_tag_handler (get_tag_handler ppf);
      set_tag_handler ppf n_tag_tag_handler;
(*      "(.n_tag." *)
      ""
    | tag -> ppf_tag_handler.tag_funs.mark_open_stag tag

  and m_tag_mark_close_tag = function
    | Format.String_tag "m" | Format.String_tag "" ->
      let ppf_tag_handler = top_tag_handler () in
      set_tag_handler ppf ppf_tag_handler;
      pop_tag_handler ();
      (*      ".m_tag.)" *)
      ""
    | Format.String_tag "n" ->
      let ppf_tag_handler = top_tag_handler () in
      set_tag_handler ppf ppf_tag_handler;
      pop_tag_handler ();
(*      ".n_tag.)" *)
      ""
    | tag -> ppf_tag_handler.tag_funs.mark_close_stag tag

  and m_tag_tag_handler = {
    tag_funs = {
      ppf_tag_handler.tag_funs with
      mark_open_stag = m_tag_mark_open_tag;
      mark_close_stag = m_tag_mark_close_tag;
    };
    out_funs = {
      ppf_tag_handler.out_funs with
      out_newline = m_tag_out_newline;
    };
    mark_tags = true;
  }

  and n_tag_tag_handler = {
    tag_funs = {
      ppf_tag_handler.tag_funs with
      mark_open_stag = m_tag_mark_open_tag;
      mark_close_stag = m_tag_mark_close_tag;
    };
    out_funs = {
      ppf_tag_handler.out_funs with
      out_newline = n_tag_out_newline;
    };
    mark_tags = true;
  } in

  pp_set_formatter_stag_functions ppf m_tag_tag_handler.tag_funs;
  pp_set_mark_tags ppf m_tag_tag_handler.mark_tags;
  ppf
;;

let make_tag_printer tnlm printer ppf ast =
  let mark_tags = pp_get_mark_tags ppf () in
  printer (make_tag_ppf tnlm ppf) ast;
  pp_set_mark_tags ppf mark_tags;
;;

let default_tnlm = {
  m_tag_new_line = " ...\n";
  n_tag_new_line = "\n";
}
;;

let make_matlab_ppf ppf = make_tag_ppf default_tnlm ppf;;

let matlab_printer printer ppf ast =
  let mark_tags = pp_get_mark_tags ppf () in
  printer (make_matlab_ppf ppf) ast;
  pp_set_mark_tags ppf mark_tags;
;;


(* Examples:

let ppf = make_matlab_ppf (Format.formatter_of_out_channel stdout);;

let p fmt = Format.fprintf ppf fmt;;

p "@[<v>@{@[<v>poi@{<n>@ @}@ poi@]@}toto@ @]@."
;;

p "@{<n>\
    toto;@{\
           @[<v>function@ \
           @{<n>\
             poipoi.@ \
             poipoi.\
           @}@ \
           toto\
           @]\
         @}\
   @}\
   @."
;;
*)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
