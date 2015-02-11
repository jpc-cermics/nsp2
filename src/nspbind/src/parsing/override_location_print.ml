(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_position ppf = function
  | l_p -> Lexing_print.print_position ppf l_p
;;

let print_t ppf = function
  | { Override_location.loc_beg = p; Override_location.loc_end = p0; } ->
    Format.fprintf ppf "%,@[<1>{@,"; Format.fprintf ppf "%,@[<1>loc_beg =@ ";
    print_position ppf p; Format.fprintf ppf "%,;@]@ ";
    Format.fprintf ppf "%,@[<1>loc_end =@ "; print_position ppf p0;
    Format.fprintf ppf "%,;@]@ "; Format.fprintf ppf "%,@,}@]"
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
