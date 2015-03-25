(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

let printer_file_name ctx ppf = function
  | s -> Og_printer.printer_String ctx ppf s

and printer_dir_name ctx ppf = function
  | s -> Og_printer.printer_String ctx ppf s
;;

let printer_path ctx ppf = function | d -> printer_dir_name ctx ppf d
;;

let printer_t ctx ppf = function | p -> printer_path ctx ppf p
;;

let printer_explicit_file_name ctx ppf = function
  | e ->
    (fun f -> printer_file_name ctx ppf f) (Path.of_explicit_file_name e)
;;

let printer_file_extension ctx ppf = function
  | f ->
    (fun s -> Og_printer.printer_String ctx ppf s) (Path.of_file_extension f)
;;

let printer_search_path ctx ppf = function
  | p_l -> Og_printer.printer_List printer_path ctx ppf p_l
;;

(* Defining top printers. *)

let print_file_name ppf = printer_file_name Og_precedence.Ck_toplevel ppf

and print_dir_name ppf = printer_dir_name Og_precedence.Ck_toplevel ppf
;;

let print_path ppf = printer_path Og_precedence.Ck_toplevel ppf
;;

let print_t ppf = printer_t Og_precedence.Ck_toplevel ppf
;;

let print_explicit_file_name ppf =
  printer_explicit_file_name Og_precedence.Ck_toplevel ppf
;;

let print_file_extension ppf =
  printer_file_extension Og_precedence.Ck_toplevel ppf
;;

let print_search_path ppf = printer_search_path Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
