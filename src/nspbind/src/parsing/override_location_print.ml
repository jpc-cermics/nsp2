(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

let printer_position ctx ppf = function
  | l_p -> Lexing_print.printer_position ctx ppf l_p
;;

let printer_t ctx ppf = function
  | { Override_location.loc_beg = p; Override_location.loc_end = p0; } ->
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%, {@ " else
    Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
    Format.fprintf ppf "%,Override_location.@,";
    Format.fprintf ppf "%,@[<hv 2>loc_beg =";
    printer_position Og_precedence.Ck_record_field ppf p;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>loc_end =";
    printer_position Og_precedence.Ck_record_field ppf p0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@;<1 -2>}" else
    Format.fprintf ppf "%,@;<1 -2>}@]"
;;

(* Defining top printers. *)

let print_position ppf = printer_position Og_precedence.Ck_toplevel ppf
;;

let print_t ppf = printer_t Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
