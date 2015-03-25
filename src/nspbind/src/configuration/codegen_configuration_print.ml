(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

let printer_version ctx ppf = function
  | s -> Og_printer.printer_String ctx ppf s
;;

let printer_source_file_types _ctx ppf = function
  | Codegen_configuration.Definition ->
    if _ctx = Og_precedence.Ck_argument ||
       _ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,Codegen_configuration.Definition"
  | Codegen_configuration.Overrides ->
    if _ctx = Og_precedence.Ck_argument ||
       _ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,Codegen_configuration.Overrides"
;;

let printer_configuration ctx ppf = function
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
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%, {@ " else
    Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
    Format.fprintf ppf "%,Codegen_configuration.@,";
    Format.fprintf ppf "%,@[<hv 2>software_name =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>software_version =";
    printer_version Og_precedence.Ck_record_field ppf v;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>verbose =";
    Og_printer.printer_Bool Og_precedence.Ck_record_field ppf b;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>debug =";
    Og_printer.printer_Bool Og_precedence.Ck_record_field ppf b0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>warnings =";
    Og_printer.printer_Bool Og_precedence.Ck_record_field ppf b1;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>source_file_basename =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>definitions_source_file =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>overrides_source_file =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o1;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>target_file =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o2;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>prefix =";
    Og_printer.printer_Option Og_printer.printer_String
      Og_precedence.Ck_record_field
      ppf
      s_o;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>path_to_override_for_c =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o3;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>path_to_override_for_h =";
    Og_printer.printer_Option Path_print.printer_file_name
      Og_precedence.Ck_record_field
      ppf
      p_f_o4;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@;<1 -2>}" else
    Format.fprintf ppf "%,@;<1 -2>}@]"
;;

(* Defining top printers. *)

let print_version ppf = printer_version Og_precedence.Ck_toplevel ppf
;;

let print_source_file_types ppf =
  printer_source_file_types Og_precedence.Ck_toplevel ppf
;;

let print_configuration ppf =
  printer_configuration Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
