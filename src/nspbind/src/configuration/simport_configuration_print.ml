(* Beginning of OCamlGen, 3.3+dev4 generated code *)

let print_translator ppf = function
  | Simport_configuration.T_simulink -> Format.fprintf ppf "%s" "T_simulink"
  | Simport_configuration.T_mtlb -> Format.fprintf ppf "%s" "T_mtlb"
  | Simport_configuration.T_sime -> Format.fprintf ppf "%s" "T_sime"
;;

let print_simulink_source_language ppf = function
  | Simport_configuration.Mdl -> Format.fprintf ppf "%s" "Mdl"
  | Simport_configuration.Slx -> Format.fprintf ppf "%s" "Slx"
  | Simport_configuration.Xml -> Format.fprintf ppf "%s" "Xml"
;;

let print_simulink_target_language ppf = function
  | Simport_configuration.ScicosLab -> Format.fprintf ppf "%s" "ScicosLab"
  | Simport_configuration.Nsp -> Format.fprintf ppf "%s" "Nsp"
  | Simport_configuration.Hypermath -> Format.fprintf ppf "%s" "Hypermath"
;;

let print_mtlb_source_language ppf = function
  | Simport_configuration.Mtlb -> Format.fprintf ppf "%s" "Mtlb"
;;

let print_mtlb_target_language ppf = function
  | Simport_configuration.Octave -> Format.fprintf ppf "%s" "Octave"
  | Simport_configuration.Matlab -> Format.fprintf ppf "%s" "Matlab"
  | Simport_configuration.Scicos_host s ->
    Format.fprintf ppf "@[<1>%s@ " "Scicos_host";
    print_simulink_target_language ppf s; Format.fprintf ppf "%,@]"
;;

let print_sime_source_language ppf = function
  | Simport_configuration.E_simulink -> Format.fprintf ppf "%s" "E_simulink"
;;

let print_sime_target_language ppf = function
  | Simport_configuration.E_scicos -> Format.fprintf ppf "%s" "E_scicos"
;;

let print_target_language ppf = function
  | Simport_configuration.Target_simulink s ->
    Format.fprintf ppf "@[<1>%s@ " "Target_simulink";
    print_simulink_target_language ppf s; Format.fprintf ppf "%,@]"
  | Simport_configuration.Target_mtlb m ->
    Format.fprintf ppf "@[<1>%s@ " "Target_mtlb";
    print_mtlb_target_language ppf m; Format.fprintf ppf "%,@]"
  | Simport_configuration.Target_sime s ->
    Format.fprintf ppf "@[<1>%s@ " "Target_sime";
    print_sime_target_language ppf s; Format.fprintf ppf "%,@]"
;;

let print_source_language ppf = function
  | Simport_configuration.Source_simulink s ->
    Format.fprintf ppf "@[<1>%s@ " "Source_simulink";
    print_simulink_source_language ppf s; Format.fprintf ppf "%,@]"
  | Simport_configuration.Source_mtlb m ->
    Format.fprintf ppf "@[<1>%s@ " "Source_mtlb";
    print_mtlb_source_language ppf m; Format.fprintf ppf "%,@]"
  | Simport_configuration.Source_sime s ->
    Format.fprintf ppf "@[<1>%s@ " "Source_sime";
    print_sime_source_language ppf s; Format.fprintf ppf "%,@]"
;;

let print_language ppf = function
  | Simport_configuration.Source_language s ->
    Format.fprintf ppf "@[<1>%s@ " "Source_language";
    print_source_language ppf s; Format.fprintf ppf "%,@]"
  | Simport_configuration.Target_language t ->
    Format.fprintf ppf "@[<1>%s@ " "Target_language";
    print_target_language ppf t; Format.fprintf ppf "%,@]"
;;

let print_natural ppf = function | u_t -> Unatural_print.print_t ppf u_t
;;

let rec print_font ppf = function
  | {
      Simport_configuration.font_name = f;
      Simport_configuration.font_angle = s;
      Simport_configuration.font_size = n;
      Simport_configuration.font_weight = s0;
    } ->
    Format.fprintf ppf "%,@[<1>{@,";
    Format.fprintf ppf "%,@[<1>font_name =@ "; print_font_name ppf f;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>font_angle =@ ";
    Lib_print.print_quoted_string ppf s; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>font_size =@ "; print_natural ppf n;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>font_weight =@ ";
    Lib_print.print_quoted_string ppf s0; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@,}@]"

and print_font_name ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_version ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_interface_version ppf = function
  | s -> Lib_print.print_quoted_string ppf s
;;

let print_interface_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_interface_marshaled_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_implementation_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_implementation_marshaled_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_implementation_translated_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_matlab_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_matlab_translated_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_mtlb_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_mtlb_translated_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_sime_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_sime_translated_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_configuration ppf = function
  | {
      Simport_configuration.software_name = s;
      Simport_configuration.software_version = v;
      Simport_configuration.interface_version = i;
      Simport_configuration.verbose = b;
      Simport_configuration.debug = b0;
      Simport_configuration.debug_slx = b1;
      Simport_configuration.warnings = b2;
      Simport_configuration.search_path = p_s;
      Simport_configuration.translator = t_o;
      Simport_configuration.model_ident = i_i_o;
      Simport_configuration.source_file = p_f_o;
      Simport_configuration.companion_source_file = p_f_o0;
      Simport_configuration.target_file = p_f_o1;
      Simport_configuration.companion_target_file = p_f_o2;
      Simport_configuration.interfaces = i_i_l;
      Simport_configuration.target_language = t_o0;
      Simport_configuration.source_language = s_o;
      Simport_configuration.window_width = n;
      Simport_configuration.window_height = n0;
      Simport_configuration.diagram_left_margin = n1;
      Simport_configuration.diagram_right_margin = n2;
      Simport_configuration.diagram_bottom_margin = n3;
      Simport_configuration.default_font = f;
      Simport_configuration.mean_blocks_by_system = n4;
      Simport_configuration.mean_block_bindings = n5;
      Simport_configuration.max_block_types = n6;
      Simport_configuration.mean_block_parameters = n7;
      Simport_configuration.block_vertical_spacing = n8;
      Simport_configuration.block_port_width = n9;
      Simport_configuration.block_port_height = n10;
      Simport_configuration.block_geometry_magnification = n11;
      Simport_configuration.not_yet_translated_mark = s0;
      Simport_configuration.model_target_ident = i_i;
    } ->
    Format.fprintf ppf "%,@[<1>{@,";
    Format.fprintf ppf "%,@[<1>software_name =@ ";
    Lib_print.print_quoted_string ppf s; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>software_version =@ "; print_version ppf v;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>interface_version =@ ";
    print_interface_version ppf i; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>verbose =@ "; Lib_print.print_bool ppf b;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>debug =@ ";
    Lib_print.print_bool ppf b0; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>debug_slx =@ "; Lib_print.print_bool ppf b1;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>warnings =@ ";
    Lib_print.print_bool ppf b2; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>search_path =@ ";
    Path_print.print_search_path ppf p_s; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>translator =@ ";
    Lib_print.print_Option print_translator ppf t_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>model_ident =@ ";
    Lib_print.print_Option Ident_print.print_ident ppf i_i_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>source_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>companion_source_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o0;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>target_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o1;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>companion_target_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o2;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>interfaces =@ ";
    Lib_print.print_List Ident_print.print_ident ppf i_i_l;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>target_language =@ ";
    Lib_print.print_Option print_target_language ppf t_o0;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>source_language =@ ";
    Lib_print.print_Option print_source_language ppf s_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>window_width =@ "; print_natural ppf n;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>window_height =@ "; print_natural ppf n0;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>diagram_left_margin =@ ";
    print_natural ppf n1; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>diagram_right_margin =@ ";
    print_natural ppf n2; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>diagram_bottom_margin =@ ";
    print_natural ppf n3; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>default_font =@ "; print_font ppf f;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>mean_blocks_by_system =@ ";
    print_natural ppf n4; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>mean_block_bindings =@ ";
    print_natural ppf n5; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>max_block_types =@ "; print_natural ppf n6;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>mean_block_parameters =@ ";
    print_natural ppf n7; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>block_vertical_spacing =@ ";
    print_natural ppf n8; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>block_port_width =@ "; print_natural ppf n9;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>block_port_height =@ "; print_natural ppf n10;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>block_geometry_magnification =@ ";
    print_natural ppf n11; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>not_yet_translated_mark =@ ";
    Lib_print.print_quoted_string ppf s0; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>model_target_ident =@ ";
    Ident_print.print_ident ppf i_i; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@,}@]"
;;

(* End of OCamlGen, 3.3+dev4 generated code *)
