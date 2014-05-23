val count_newlines : string -> int
val get_include_code : string -> string -> string -> string
val write_header_file :
  Stringarg.object_rec -> bool -> string -> (string, string) Hashtbl.t -> unit
