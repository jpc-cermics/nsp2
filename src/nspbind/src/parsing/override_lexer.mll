(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2015,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{

(* Prelude part: this is pure Caml. *)

(* open Override_ast;; *)

open Override_parser;;
open Lexing;;

(** {6 Lexing errors} *)

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Test_error
;;

(** The various errors when lexing. *)

exception Lexer_error of error * Lexing.position * Lexing.position;;

let error (reason, start_p, curr_p) =
  raise (Lexer_error (reason, start_p, curr_p))
;;

(** {6 Explaining lexing errors} *)

let report_error ppf = function
  | Illegal_character c ->
      Format.fprintf ppf "Illegal character %C" c
  | Unterminated_comment ->
      Format.fprintf ppf "Unterminated comment"
  | Unterminated_string ->
      Format.fprintf ppf "Unterminated string"
  | Test_error -> Format.fprintf ppf "Test location\n"
;;

let report_lexical_error ppf = function
  | Lexer_error (r, sp, ep) ->
    let loc = Override_location.mk_loc sp ep in
    Format.fprintf ppf
      "%a@.Lexical error: %a@."
      Override_location.print loc
      report_error r
  | exn -> raise exn
;;

let debug = false;;

(* Push the character `c' back in the `lexbuf'.  Since ocamllex does not
 * implement this functionality, I made it myself.  I used the `dump_lex'
 * function above to track the internal state of the lexer and see how it
 * was used and implemented push_back accordingly.  But since I'm not sure
 * this reverse engineering process was 100% right, I left some assertions
 * and the code of dump_lex above.
 * This code will b0rk everything up if you push_back something different
 * then what has been lexed by the last semantic action of ocamllex.  You
 * were warned.
 * from revcpp Quentin Hocquet   <mefyl@lrde.epita.fr>
 * Benoit Sigoure    <tsuna@lrde.epita.fr>
 *)

let push_back lexbuf c =
  if debug then prerr_endline (Printf.sprintf "push back a character =[%s]" (String.make 1 c));
  let fail msg =
    print_string msg
  in
    if lexbuf.lex_curr_pos = 0 then
      (* The buffer was flushed so we need to really push back `c' in the
         buffer.  *)
      (lexbuf.lex_buffer <- (String.make 1 c) ^ lexbuf.lex_buffer;
       lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + 1);

    (* Now `c' is in the buffer, simply change the current positions.  *)
    (* Perform some sanity checks:
       1. The buffer_len can't be 0 since our `c' must still be in it.
       2. start_pos, curr_pos and last_pos must not be 0 since we're going
          to decrement them.
       3. Ditto for start_p.pos_cnum and curr_p.pos_cnum.
    *)
    if lexbuf.lex_buffer_len = 0
    then fail ("Assertion failed: lex_curr_pos = "
                ^ string_of_int lexbuf.lex_curr_pos
                ^ " and lex_buffer_len = 0");
    if lexbuf.lex_curr_pos <= 0
    then fail ("Assertion failed: lex_curr_pos <= 0");
    if lexbuf.lex_curr_p.pos_cnum <= 0
    then fail ("Assertion failed: lex_curr_p.pos_cnum <= 0");
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
    lexbuf.lex_last_pos <- lexbuf.lex_last_pos - 1;
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1 }
;;
(* Push the string `s' back in the `lexbuf'.  The comments of push_back
 *  above also apply here.  You were warned.
 *)

let push_back_strings lexbuf = function
  [] -> ()
  | elt::elts -> String.iter (push_back lexbuf) elt;
      List.iter (function arg -> String.iter (push_back lexbuf) arg) elts
;;

(** {6 Keeping the internal buffer locations up to date} *)

let update_loc lexbuf fname line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_fname =
    match fname with
    | None -> pos.pos_fname
    | Some s -> s in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = new_fname;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(** Add one to the current line counter of the file being lexed. *)

let incr_line_num lexbuf =
  update_loc lexbuf None 1 false 0
;;

(** Add number of newlines contained in string to the current line
    counter of the file being lexed. *)

let count_newlines s = 
  let c = ref 0 in
  for i = 0 to (String.length s) - 1 do
    if s.[i] == '\n' then
      c := !c +1
  done;
  !c
;;

let incr_lines lexbuf str =
  let count = (count_newlines str) in
  update_loc lexbuf None count false 0
;;

(* debug *)

let show_location str lexbuf =
  let loc = Override_location.mk_loc  lexbuf.lex_start_p lexbuf.lex_curr_p in
  Format.fprintf Format.std_formatter "%a@.Test Lexical error: %s %a@."
    Override_location.print loc str report_error Test_error

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 128000;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
;;

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    if true then  prerr_endline 
      (Printf.sprintf "Buffer size increased");
    let new_buff = Bytes.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  Bytes.unsafe_set (!string_buff) (!string_index) c;
  incr string_index
;;

let store_string s = 
  if debug then  prerr_endline 
      (Printf.sprintf "store a string [%s]\n" s);
  String.iter store_string_char s
;;

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s
;;

let print_lexemes = function
  | [] -> ()
  | elt::elts -> prerr_endline (Printf.sprintf  "%s" elt);
      List.iter (function arg -> prerr_endline (Printf.sprintf  "%s" arg)) elts
;;

let split_string str = 
  let str = str (* String.trim str *) in 
  let bb = Buffer.create 1024 in 
  String.iter 
    (fun x -> 
      match x with 
      | '\n' 
      | ' ' -> 
	  let n = Buffer.length bb in 
	  if n = 0 then 
	    Buffer.add_char bb ' '
	  else
	    let c = Buffer.nth bb (n-1) in 
	    if c <> ' ' then 
	      Buffer.add_char bb ' ';
      | c -> Buffer.add_char bb c
    ) str;
  let str = Buffer.contents bb in 
  let str = 
    if str.[0] = ' ' then 
      (String.sub str 1 ((String.length str) -1))
    else 
      str in 
  if str.[(String.length str) -1] = ' ' then 
    (String.sub str 0 ((String.length str) -1))								       
  else
    str 
;;

}


(* start of lexer rules *)

(* comments up to end of line, block comments are defined latter *)

let lowercase = [ 'a' - 'z' ]
let uppercase = [ 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let identchar = [ 'a' - 'z'   'A' - 'Z' '_' '-' ]
let ident = identchar ( digit | identchar ) * 

(* override token delimiters *)

let start_rule = "%%"
let blank = [ ' ' '\t' ]
let eol_char = ('\n' | ('\r' '\n'))
let not_eol_char = ([^ '\n' '\r' ] | ('\r' [^ '\n' ]))
let newline = blank* eol_char ( blank | eol_char ) *

(* code *) 

let code_str = [ ^ '%']* '%' '%'

(* tokenizers *)

rule token = parse

| start_rule blank* eol_char (ident as s1 ) blank+ ((ident as s2) '.' (ident as s3)) (not_eol_char* as s4) eol_char
    {
     incr_line_num lexbuf;
     incr_line_num lexbuf;
     if debug then 
       prerr_endline 
	 (Printf.sprintf "--> start-section1 ->[%s (%s dot %s)]" s1 s2 s3);
     reset_string_buffer ();
     code lexbuf;
     let str = get_stored_string() in
     if debug then prerr_endline (Printf.sprintf "<-- code2=[%s]" str);
     incr_lines lexbuf str;
     if s1 = "ignore" || s1 = "ignore-glob" || s1 = "import" then 
       (RULE2(s1, split_string (s2 ^ "." ^ s3 ^ s4 ^ " " ^ str)))
     else
       (RULE4(s1,s2,s3,str));
   }

| start_rule blank* eol_char (ident as s1 ) blank+ (ident as s2) (not_eol_char* as rest) eol_char
    {
     incr_line_num lexbuf;
     incr_line_num lexbuf;
     if debug then 
       prerr_endline 
	 (Printf.sprintf "--> start-section2 ->[%s (%s) [%s]]" s1 s2 rest);
     reset_string_buffer ();
     code lexbuf;
     let str = get_stored_string() in
     if debug then prerr_endline (Printf.sprintf "<-- code3=[%s]" str);
     incr_lines lexbuf str;
     if s1 = "ignore" || s1 = "ignore-glob" || s1 = "import" then 
       (
	RULE2(s1, split_string (s2 ^ rest ^ " " ^ str))
       )
     else
       (
	RULE3(s1,s2,str)
       );
      }

| start_rule blank* eol_char (ident as s ) (not_eol_char* as s2) eol_char
    {
     incr_line_num lexbuf;
     incr_line_num lexbuf;
     if debug then 
       prerr_endline 
	 (Printf.sprintf "--> start-section3 ->[%s empty]" s);
     reset_string_buffer ();
     code lexbuf;
     let str = get_stored_string() in
     incr_lines lexbuf str;
     let str = 
       if s = "ignore" || s = "ignore-glob" || s = "import" then 
	 (
	  split_string (s2 ^ " " ^ str) 
	 )
       else
	 str in 
     if debug then prerr_endline (Printf.sprintf "<-- code4=[%s]" str);
     RULE2(s,str);
   }

|  [ ^ '%']* '%' '%' ( blank | eol_char )* eof 
    {
     (* this can happen if file is ended by an empty %% 
      * we add this rule to avoid infinite loop on next one 
      *)
     EOF
   }

| ( [ ^ '%']* as s) '%' '%' 
    {
     push_back lexbuf '%';
     push_back lexbuf '%';
     incr_lines lexbuf s;
     if debug then prerr_endline (Printf.sprintf "<-- start=[%s]" s);
     RULE2("start",s);
   }

| eof 
    {
     EOF
   }
| _
    { 
      prerr_endline (Printf.sprintf "autre");
      error
        (Illegal_character (Lexing.lexeme_char lexbuf 0),
         lexbuf.lex_start_p,
         lexbuf.lex_curr_p) }

and items = parse 
|  (ident as s)
    {
     if debug then 
       prerr_endline 
	 (Printf.sprintf "--> NAME ->[%s]" s );
     NAME(s)
   }
| (blank | eol_char)* 
    {
     if debug then 
       prerr_endline 
	 (Printf.sprintf "--> blank" );
     token lexbuf
   }

(* code part: 
 * Note that the last rule _ will slow the code 
 * if used on a large number of characters 
 * That's why we have added two rules 
 *)

and code = parse
  | ([ ^ '%']* as s) eof 
      {
        if debug then 
	 prerr_endline 
	    (Printf.sprintf "get code up to end of file");
       store_string s;
     }

  | ([ ^ '%']* as s) '%' '%' 
      {
       if debug then 
	 prerr_endline 
	   (Printf.sprintf "stop code at start of a potential new section");
       store_string s; 

       if String.length s = 0 || s.[(String.length s) - 1] = '\n' then 
	 (
	  if debug then  prerr_endline 
	      (Printf.sprintf "real end of code found ");
	  push_back lexbuf '%';
	  push_back lexbuf '%';
	 )
       else
	 ( 
	   if debug then prerr_endline 
	       (Printf.sprintf "the two percent were inside the code");
	   store_string_char '%';
	   store_string_char '%';
	   code lexbuf;
	  );
     }
 | ([ ^ '%']* '%' as s)
      {
       if debug then 
	 prerr_endline 
	   (Printf.sprintf "stop at %% store string and go on");
       store_string s;
       code lexbuf;
     }

  | eof 
      {
       if debug then 
	 prerr_endline 
	   (Printf.sprintf "end of file " );
       }
  | _
      { 
	if debug then prerr_endline 
	    (Printf.sprintf "store one character");
	store_string_char(Lexing.lexeme_char lexbuf 0);
        code lexbuf 
      }

{

(* zone for extra functions *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)

}
