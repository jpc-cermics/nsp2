(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
type translator =
  | T_simulink
  | T_mtlb
  | T_sime
;;

type simulink_source_language =
  | Mdl
  | Slx
  | Xml
;;
type simulink_target_language =
  | ScicosLab
  | Nsp
  | Hypermath
;;

type mtlb_source_language =
  | Mtlb
;;
type mtlb_target_language =
  | Octave
  | Matlab
  | Scicos_host of simulink_target_language
;;

type sime_source_language =
  | E_simulink
;;
type sime_target_language =
  | E_scicos
;;

type target_language =
  | Target_simulink of simulink_target_language
  | Target_mtlb of mtlb_target_language
  | Target_sime of sime_target_language
;;

type source_language =
  | Source_simulink of simulink_source_language
  | Source_mtlb of mtlb_source_language
  | Source_sime of sime_source_language
;;

type language =
  | Source_language of source_language
  | Target_language of target_language
;;

type natural = Unatural.t;;

type font = {
  font_name : font_name;
  font_angle : string;
  font_size : natural;
  font_weight : string;
}

and font_name = string
;;

type version = string;;

type interface_version = string;;

type interface_source_file_name = Path.explicit_file_name;;
type interface_marshaled_file_name = Path.explicit_file_name;;

type implementation_source_file_name = Path.explicit_file_name;;
type implementation_marshaled_file_name = Path.explicit_file_name;;

type implementation_translated_file_name = Path.explicit_file_name;;

type matlab_source_file_name = Path.explicit_file_name;;
type matlab_translated_file_name = Path.explicit_file_name;;

type mtlb_source_file_name = Path.explicit_file_name;;
type mtlb_translated_file_name = Path.explicit_file_name;;

type sime_source_file_name = Path.explicit_file_name;;
type sime_translated_file_name = Path.explicit_file_name;;

type configuration = {

          software_name : string;
          software_version : version;
          interface_version : interface_version;

  mutable verbose : bool;
  mutable debug : bool;
  mutable debug_slx : bool;
  mutable warnings : bool;

  mutable search_path : Path.search_path;

  mutable translator : translator option;

  mutable model_ident : Ident.ident option;

  mutable source_file : Path.file_name option;
  mutable companion_source_file : Path.file_name option;

  mutable target_file : Path.file_name option;
  mutable companion_target_file : Path.file_name option;

  mutable interfaces : Ident.ident list;

  mutable target_language : target_language option;
  mutable source_language : source_language option;

  mutable window_width : natural;
  mutable window_height : natural;

  mutable diagram_left_margin : natural;
  mutable diagram_right_margin : natural;
  mutable diagram_bottom_margin : natural;

  mutable default_font : font;

          mean_blocks_by_system : natural;
          mean_block_bindings : natural;
          max_block_types : natural;
          mean_block_parameters : natural;

  mutable block_vertical_spacing : natural;
  mutable block_port_width : natural;
  mutable block_port_height : natural;
  mutable block_geometry_magnification : natural;

  mutable not_yet_translated_mark : string;

  mutable model_target_ident : Ident.ident;
}
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
