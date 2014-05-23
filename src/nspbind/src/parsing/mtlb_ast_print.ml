(* Beginning of ocamlprintc-3.3+dev4 generated code *)

open Ast_node_print;;

let print_parsing ppf = function
  | Mtlb_ast.ParsingMtlb -> Format.fprintf ppf "%s" "ParsingMtlb"
;;

let rec print_implementation_file pp_info ppf = function
  | i_i_i_a ->
    print_ast_node (print_implementation_file_desc pp_info) pp_info ppf
     i_i_i_a

and print_implementation_file_desc pp_info ppf = function
  | Mtlb_ast.Ast i_o_l ->
    Format.fprintf ppf "@[<1>%s@ " "Ast";
    Lib_print.print_List (print_override pp_info) ppf i_o_l;
    Format.fprintf ppf "%,@]"

and print_override pp_info ppf = function
  | i_o_i_a ->
    print_ast_node (print_override_desc pp_info) pp_info ppf i_o_i_a

and print_override_desc _pp_info ppf = function
  | Mtlb_ast.Ignore s_l ->
    Format.fprintf ppf "@[<1>%s@ " "Ignore";
    Lib_print.print_List Lib_print.print_quoted_string ppf s_l;
    Format.fprintf ppf "%,@]"
  | Mtlb_ast.Override2 (s, s0) ->
    Format.fprintf ppf "@[<1>%s@ " "Override2";
    Format.fprintf ppf "%,@[<1>(@,"; Lib_print.print_quoted_string ppf s;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s0;
    Format.fprintf ppf "%,@,)@]"; Format.fprintf ppf "%,@]"
  | Mtlb_ast.Override3 (s, s0, s1) ->
    Format.fprintf ppf "@[<1>%s@ " "Override3";
    Format.fprintf ppf "%,@[<1>(@,"; Lib_print.print_quoted_string ppf s;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s0;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s1;
    Format.fprintf ppf "%,@,)@]"; Format.fprintf ppf "%,@]"
  | Mtlb_ast.Override4 (s, s0, s1, s2) ->
    Format.fprintf ppf "@[<1>%s@ " "Override4";
    Format.fprintf ppf "%,@[<1>(@,"; Lib_print.print_quoted_string ppf s;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s0;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s1;
    Format.fprintf ppf "%,,@ "; Lib_print.print_quoted_string ppf s2;
    Format.fprintf ppf "%,@,)@]"; Format.fprintf ppf "%,@]"
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
