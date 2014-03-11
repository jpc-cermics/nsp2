(* Beginning of ocamlprintc-3.3+dev4 generated code *)

let print_list1 pp_a ppf = function
  | (a, a_l) ->
    Format.fprintf ppf "%,@[<1>(@,"; pp_a ppf a; Format.fprintf ppf "%,,@ ";
    Lib_print.print_List pp_a ppf a_l; Format.fprintf ppf "%,@,)@]"
;;

let print_t pp_a ppf = function | a_l -> print_list1 pp_a ppf a_l
;;

(* End of ocamlprintc-3.3+dev4 generated code *)
