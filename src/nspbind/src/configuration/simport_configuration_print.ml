(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_version ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_definitions_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_overrides_source_file_name ppf = function
  | p_e -> Path_print.print_explicit_file_name ppf p_e
;;

let print_source_file_types ppf = function
  | Simport_configuration.Definition -> Format.fprintf ppf "%s" "Definition"
  | Simport_configuration.Overrides -> Format.fprintf ppf "%s" "Overrides"
;;

let print_configuration ppf = function
  | {
      Simport_configuration.software_name = s;
      Simport_configuration.software_version = v;
      Simport_configuration.verbose = b;
      Simport_configuration.debug = b0;
      Simport_configuration.warnings = b1;
      Simport_configuration.source_file_basename = p_f_o;
      Simport_configuration.definitions_source_file = p_f_o0;
      Simport_configuration.overrides_source_file = p_f_o1;
      Simport_configuration.target_file = p_f_o2;
      Simport_configuration.prefix = s_o;
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
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@,}@]"
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
