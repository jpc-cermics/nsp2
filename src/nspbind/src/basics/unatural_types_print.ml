(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_natural ppf = function
  | n ->
    (fun i -> Lib_print.print_quoted_int ppf i) (Unatural_types.of_natural n)
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
