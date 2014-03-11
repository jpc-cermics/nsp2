(* Beginning of ocamlprintc-3.3+dev4 generated code *)

open Ident_print;;

let rec print_ident_binding pp_a ppf = function
  | { Binding.bound_ident = i; Binding.bound_value = a; } ->
    Format.fprintf ppf "%,@[<1>{@,";
    Format.fprintf ppf "%,@[<1>bound_ident =@ "; print_ident ppf i;
    Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>bound_value =@ "; pp_a ppf a;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@,}@]"

and print_ident_bindings pp_a ppf = function
  | a_i_l -> Lib_print.print_List (print_ident_binding pp_a) ppf a_i_l

and print_ident_binding_table pp_a ppf = function
  | i_a_i_h_t ->
    Hash_table_print.print_t print_ident (print_ident_binding pp_a) ppf
     i_a_i_h_t
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
