(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_ast_node pp_desc pp_info ppf = function
  | { Ast_node.ast_loc = o_t; Ast_node.ast_desc = d; Ast_node.ast_info = i; } ->
     Format.fprintf ppf "%,@[<1>{@,";
     Format.fprintf ppf "%,@[<1>ast_loc =@ ";
     Override_location_print.print_t ppf o_t; Format.fprintf ppf "%,;@]@ ";
     Format.fprintf ppf "%,@[<1>ast_desc =@ "; pp_desc ppf d;
     Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@[<1>ast_info =@ ";
     pp_info ppf i; Format.fprintf ppf "%,;@]@ ";
     Format.fprintf ppf "%,@,}@]"
;;

let print_t pp_desc pp_info ppf = function
  | d_i_a -> print_ast_node pp_desc pp_info ppf d_i_a
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
