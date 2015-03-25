(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

let printer_ast_node pp_desc pp_info ctx ppf = function
  | { Ast_node.ast_loc = o_t; Ast_node.ast_desc = d; Ast_node.ast_info = i; } ->
     if ctx = Og_precedence.Ck_argument ||
        ctx = Og_precedence.Ck_record_field
       then Format.fprintf ppf "%, {@ " else
     Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
     Format.fprintf ppf "%,Ast_node.@,";
     Format.fprintf ppf "%,@[<hv 2>ast_loc =";
     Override_location_print.printer_t Og_precedence.Ck_record_field ppf o_t;
     Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
     Format.fprintf ppf "%,@[<hv 2>ast_desc =";
     pp_desc Og_precedence.Ck_record_field ppf d; Format.fprintf ppf "%,;@]";
     Format.fprintf ppf "%,@ "; Format.fprintf ppf "%,@[<hv 2>ast_info =";
     pp_info Og_precedence.Ck_record_field ppf i; Format.fprintf ppf "%,;@]";
     Format.fprintf ppf "%,@]";
     if ctx = Og_precedence.Ck_argument ||
        ctx = Og_precedence.Ck_record_field
       then Format.fprintf ppf "%,@;<1 -2>}" else
     Format.fprintf ppf "%,@;<1 -2>}@]"
;;

let printer_t pp_desc pp_info ctx ppf = function
  | d_i_a -> printer_ast_node pp_desc pp_info ctx ppf d_i_a
;;

(* Defining top printers. *)

let print_ast_node pp_desc pp_info ppf =
  printer_ast_node pp_desc pp_info Og_precedence.Ck_toplevel ppf
;;

let print_t pp_desc pp_info ppf =
  printer_t pp_desc pp_info Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
