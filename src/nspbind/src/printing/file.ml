
type file_output = 
    {
     mutable ppf : Format.formatter;
     mutable filename : string;
     mutable override_c_filename : string;
     mutable override_h_filename : string;
     mutable override_filename : string;
     mutable lineno :  int;
   }
;;

let file_output = 
  {
   ppf = Format.std_formatter;
   filename = "";
   override_c_filename = "";
   override_h_filename = "";
   override_filename = "";
   lineno = 0;
 }

let set_ppf ppf = 
  file_output.ppf <- ppf
;;

let set_c_override () = 
  file_output.override_filename <- file_output.override_c_filename
;;

let set_h_override () = 
  file_output.override_filename <- file_output.override_h_filename
;;

let set_filenames filename override_c_filename override_h_filename= 
  file_output.filename <- filename;
  file_output.override_c_filename <- override_c_filename;
  file_output.override_h_filename <- override_h_filename;
  set_c_override ();
;;

let setline linenum filename = 
  Format.fprintf file_output.ppf "#line %d \"%s\"\n" 
    linenum filename;
  file_output.lineno <- file_output.lineno + 1;
;;

let setline_override linenum = 
  setline linenum file_output.override_filename
;;

let resetline () = 
  setline (file_output.lineno+2) file_output.filename;
;;

let rec count_newlines s =
  let n = (String.length s) in
  if n = 0 then 0
  else
    let rest = count_newlines (String.sub s 1 (n -1)) in
    if s.[0] == '\n' then
      rest + 1
    else
      rest
;;

let write str = 
  Format.fprintf file_output.ppf "%s" str;
  file_output.lineno <-  file_output.lineno + (count_newlines str)
;;

let pattern_to_code pattern hash = 
  let folder key value init = 
    Str.global_replace 
      (Str.regexp_string (Printf.sprintf "%%(%s)s" key)) value init in 
  Hashtbl.fold folder hash pattern 
;;

let write_string str =  write str
;;

let write_pattern pattern hash =  
  write (pattern_to_code pattern hash);
;;

let write_override_pattern keyword name substdict = 
  let code, lineno = Overrides.get keyword name in 
  setline_override lineno;
  write_pattern code substdict;
  resetline();
;;

let write_override keyword name = 
  let code, lineno = Overrides.get keyword name in 
  setline_override lineno;
  write_string code;
  resetline();
;;

let get_override_c_file_name () = 
  file_output.override_c_filename
;;

let get_override_h_file_name () = 
  file_output.override_h_filename
;;


let bb = Buffer.create 10240;;

let pattern_to_code_with_buffer pattern hash = 
  Buffer.clear bb;
  let subst str = 
    try 
      Hashtbl.find hash str 
    with 
    | _ -> Say.fatal_error 
	  (Printf.sprintf "%s not found in replacement table" str) in 
  Buffer.add_substitute bb subst  pattern;
  Buffer.contents bb
;;

(* used to fix the line-code number *) 

let write_substitute code = 
  let line = file_output.lineno in 
  Buffer.clear bb;
  let subst str = 
    match str with 
    | "nl" -> 
	file_output.lineno <- file_output.lineno + 1;
	"\n"
    | "line" -> 
	Printf.sprintf "#line %d \"%s\"" (file_output.lineno + 2 ) file_output.filename;
    | _ -> "BUG" in 
  Buffer.add_substitute bb subst code; 
  file_output.lineno <- line;
  write_string (Buffer.contents bb);
;;

let code_with_nl code = 
  Buffer.clear bb;
  for i = 0 to String.length code - 1 do
    match code.[i] with 
    | '\n' -> Buffer.add_string bb "$(nl)"
    | c -> Buffer.add_char bb c
  done;
  Buffer.contents bb
;;

let write_substitute_pattern code hash = 
  let line = file_output.lineno in 
  let code =  code_with_nl code in 
  Buffer.clear bb;
  let subst str = 
    match str with 
    | "nl" -> 
	file_output.lineno <- file_output.lineno + 1;
	"\n"
    | "line" -> 
	Printf.sprintf "#line %d \"%s\"" (file_output.lineno + 2 ) file_output.filename;
    | str -> 
	try 
	  let str = Hashtbl.find hash str in 
	  file_output.lineno <- file_output.lineno + (count_newlines str);
	  str 
	with 
	| _ -> Say.fatal_error 
	      (Printf.sprintf "%s not found in replacement table" str) in 
  Buffer.add_substitute bb subst code; 
  file_output.lineno <- line;
  write_string (Buffer.contents bb);
;;
    
(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
