(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_version ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_source_file_types ppf = function
  | Codegen_configuration.Definition -> Format.fprintf ppf "%s" "Definition"
  | Codegen_configuration.Overrides -> Format.fprintf ppf "%s" "Overrides"
;;

let print_configuration ppf = function
  | {
      Codegen_configuration.software_name = s;
      Codegen_configuration.software_version = v;
      Codegen_configuration.verbose = b;
      Codegen_configuration.debug = b0;
      Codegen_configuration.warnings = b1;
      Codegen_configuration.source_file_basename = p_f_o;
      Codegen_configuration.definitions_source_file = p_f_o0;
      Codegen_configuration.overrides_source_file = p_f_o1;
      Codegen_configuration.target_file = p_f_o2;
      Codegen_configuration.prefix = s_o;
      Codegen_configuration.path_to_override_for_c = p_f_o3;
      Codegen_configuration.path_to_override_for_h = p_f_o4;
    } ->
    Format.fprintf ppf "%,@[<1>{@,";
    Format.fprintf ppf "%,@[<1>software_name =@ ";
    Lib_print.print_quoted_string ppf s; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>software_version =@ "; print_version ppf v;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>verbose =@ ";
    Lib_print.print_bool ppf b; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>debug =@ "; Lib_print.print_bool ppf b0;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>warnings =@ ";
    Lib_print.print_bool ppf b1; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>source_file_basename =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>definitions_source_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o0;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>overrides_source_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o1;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>target_file =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o2;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>prefix =@ ";
    Lib_print.print_Option Lib_print.print_quoted_string ppf s_o;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>path_to_override_for_c =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o3;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>path_to_override_for_h =@ ";
    Lib_print.print_Option Path_print.print_file_name ppf p_f_o4;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@,}@]"
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
