(* Beginning of OCamlGen, 4.0+dev0 generated code. *)

let printer_position ctx ppf = function
  | {
      Lexing.pos_fname = s;
      Lexing.pos_lnum = i;
      Lexing.pos_bol = i0;
      Lexing.pos_cnum = i1;
    } ->
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%, {@ " else
    Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
    Format.fprintf ppf "%,@[<hv 2>pos_fname =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>pos_lnum =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>pos_bol =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>pos_cnum =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i1;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@;<1 -2>}" else
    Format.fprintf ppf "%,@;<1 -2>}@]"
;;

let rec printer_lexbuf ctx ppf = function
  | {
      Lexing.refill_buff = f_l_u;
      Lexing.lex_buffer = b;
      Lexing.lex_buffer_len = i;
      Lexing.lex_abs_pos = i0;
      Lexing.lex_start_pos = i1;
      Lexing.lex_curr_pos = i2;
      Lexing.lex_last_pos = i3;
      Lexing.lex_last_action = i4;
      Lexing.lex_eof_reached = b0;
      Lexing.lex_mem = i_a;
      Lexing.lex_start_p = p;
      Lexing.lex_curr_p = p0;
    } ->
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%, {@ " else
    Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
    Format.fprintf ppf "%,@[<hv 2>refill_buff =";
    Og_printer.printer_Fun printer_lexbuf Og_printer.printer_Unit
      Og_precedence.Ck_record_field
      ppf
      f_l_u;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_buffer =";
    Og_printer.printer_Bytes Og_precedence.Ck_record_field ppf b;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_buffer_len =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_abs_pos =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_start_pos =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i1;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_curr_pos =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i2;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_last_pos =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i3;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_last_action =";
    Og_printer.printer_Int Og_precedence.Ck_record_field ppf i4;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_eof_reached =";
    Og_printer.printer_Bool Og_precedence.Ck_record_field ppf b0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_mem =";
    Og_printer.printer_Array Og_printer.printer_Int
      Og_precedence.Ck_record_field
      ppf
      i_a;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_start_p =";
    printer_position Og_precedence.Ck_record_field ppf p;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_curr_p =";
    printer_position Og_precedence.Ck_record_field ppf p0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@;<1 -2>}" else
    Format.fprintf ppf "%,@;<1 -2>}@]"
;;

let printer_lex_tables ctx ppf = function
  | {
      Lexing.lex_base = s;
      Lexing.lex_backtrk = s0;
      Lexing.lex_default = s1;
      Lexing.lex_trans = s2;
      Lexing.lex_check = s3;
      Lexing.lex_base_code = s4;
      Lexing.lex_backtrk_code = s5;
      Lexing.lex_default_code = s6;
      Lexing.lex_trans_code = s7;
      Lexing.lex_check_code = s8;
      Lexing.lex_code = s9;
    } ->
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%, {@ " else
    Format.fprintf ppf "%,@[<hv 2>{@ "; Format.fprintf ppf "%,@[<hv 0>";
    Format.fprintf ppf "%,@[<hv 2>lex_base =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_backtrk =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s0;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_default =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s1;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_trans =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s2;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_check =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s3;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_base_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s4;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_backtrk_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s5;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_default_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s6;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_trans_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s7;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_check_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s8;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@ ";
    Format.fprintf ppf "%,@[<hv 2>lex_code =";
    Og_printer.printer_String Og_precedence.Ck_record_field ppf s9;
    Format.fprintf ppf "%,;@]"; Format.fprintf ppf "%,@]";
    if ctx = Og_precedence.Ck_argument || ctx = Og_precedence.Ck_record_field
      then Format.fprintf ppf "%,@;<1 -2>}" else
    Format.fprintf ppf "%,@;<1 -2>}@]"
;;

(* Defining top printers. *)

let print_position ppf = printer_position Og_precedence.Ck_toplevel ppf
;;

let print_lexbuf ppf = printer_lexbuf Og_precedence.Ck_toplevel ppf
;;

let print_lex_tables ppf = printer_lex_tables Og_precedence.Ck_toplevel ppf
;;

(* End of OCamlGen, 4.0+dev0 generated code. *)
