(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_ident ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_identflag ppf = function
  | Ident.Ident i ->
    Format.fprintf ppf "@[<1>%s@ " "Ident"; print_ident ppf i;
    Format.fprintf ppf "%,@]"
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
