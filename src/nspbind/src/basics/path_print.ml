(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_file_name ppf = function | s -> Lib_print.print_quoted_string ppf s

and print_dir_name ppf = function | s -> Lib_print.print_quoted_string ppf s
;;

let print_path ppf = function | d -> print_dir_name ppf d
;;

let print_t ppf = function | p -> print_path ppf p
;;

let print_explicit_file_name ppf = function
  | e -> (fun f -> print_file_name ppf f) (Path.of_explicit_file_name e)
;;

let print_file_extension ppf = function
  | f ->
    (fun s -> Lib_print.print_quoted_string ppf s) (Path.of_file_extension f)
;;

let print_search_path ppf = function
  | p_l -> Lib_print.print_List print_path ppf p_l
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
