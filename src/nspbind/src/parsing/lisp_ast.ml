type ('a, 'b) cell = { mutable car: 'a; mutable cdr: 'b }

type t = Atom of string
         | Cons of (t, t) cell
         | Null
             
let car o =
  match o with
  | Cons (c) -> c.car
  | Null 
  | Atom _  -> invalid_arg "Argument to car must be a Cons!"

let cdr o =
  match o with
  | Cons (c) -> c.cdr
  | Atom _ 
  | Null -> invalid_arg "Argument to cdr must be a Cons!"

let cons first second = Cons { car = first ; cdr = second }
  
let name o =
  match o with
  | Atom (s) -> s
  | Cons _
  | Null -> invalid_arg "Argument to name must be an Atom!"
 
