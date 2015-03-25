(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

open Ast_node_print;;

let printer_parsing _ctx ppf = function
  | Override_ast.ParsingMtlb ->
    if _ctx = Og_precedence.Ck_argument ||
       _ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,Override_ast.ParsingMtlb"
;;

let rec printer_implementation_file pp_info ctx ppf = function
  | i_i_i_a ->
    printer_ast_node (printer_implementation_file_desc pp_info) pp_info
      ctx
      ppf
      i_i_i_a

and printer_implementation_file_desc pp_info ctx ppf = function
  | Override_ast.Ast i_o_l ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Override_ast.Ast";
    Og_printer.printer_List (printer_override pp_info)
      Og_precedence.Ck_argument
      ppf
      i_o_l;
    Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"

and printer_override pp_info ctx ppf = function
  | i_o_i_a ->
    printer_ast_node (printer_override_desc pp_info) pp_info ctx ppf i_o_i_a

and printer_override_desc _pp_info ctx ppf = function
  | Override_ast.Ignore s_l ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Override_ast.Ignore";
    Og_printer.printer_List Og_printer.printer_String
      Og_precedence.Ck_argument
      ppf
      s_l;
    Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
  | Override_ast.Override2 (s, s0) ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Override_ast.Override2";
    Format.fprintf ppf "%, (@,"; Format.fprintf ppf "%,@[<hv 0>";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s0;
    Format.fprintf ppf "%,@]"; Format.fprintf ppf "%,@;<0 -2>)";
    Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
  | Override_ast.Override3 (s, s0, s1) ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Override_ast.Override3";
    Format.fprintf ppf "%, (@,"; Format.fprintf ppf "%,@[<hv 0>";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s0;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s1;
    Format.fprintf ppf "%,@]"; Format.fprintf ppf "%,@;<0 -2>)";
    Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
  | Override_ast.Override4 (s, s0, s1, s2) ->
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%, (@," else
    if ctx = Og_precedence.Ck_record_field then Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>Override_ast.Override4";
    Format.fprintf ppf "%, (@,"; Format.fprintf ppf "%,@[<hv 0>";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s0;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s1;
    Format.fprintf ppf "%,,@ ";
    Og_printer.printer_String Og_precedence.Ck_tuple_item ppf s2;
    Format.fprintf ppf "%,@]"; Format.fprintf ppf "%,@;<0 -2>)";
    Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument then Format.fprintf ppf "%,@;<0 -2>)"
;;

(* Defining top printers. *)

let print_parsing ppf = printer_parsing Og_precedence.Ck_toplevel ppf
;;

let print_implementation_file pp_info ppf =
  printer_implementation_file pp_info Og_precedence.Ck_toplevel ppf

and print_implementation_file_desc pp_info ppf =
  printer_implementation_file_desc pp_info Og_precedence.Ck_toplevel ppf

and print_override pp_info ppf =
  printer_override pp_info Og_precedence.Ck_toplevel ppf

and print_override_desc pp_info ppf =
  printer_override_desc pp_info Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
