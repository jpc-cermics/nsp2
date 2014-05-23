
let varlist_table = Hashtbl.create 256;;

let add_var ctype name = 
  Hashtbl.add varlist_table ctype name;;

let test = 
  add_var "int" "x";
  add_var "double" "z";
  add_var "int" "w=67"
;;

let print_vars ppf = 
  let iter_key key value = 
    fprintf ppf "%s %s\n" key value in 
  Hashtbl.iter iter_key varlist_table
;;
