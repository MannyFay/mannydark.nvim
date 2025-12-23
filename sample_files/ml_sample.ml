(* ============================================================================
   Comprehensive OCaml Sample - Syntax Highlighting Demonstration
   ============================================================================ *)

(** Module documentation comment
    This module demonstrates OCaml's rich type system and features *)

(* ============================================================================
   Numeric Literals and Basic Types
   ============================================================================ *)

(* Integer literals *)
let integer_literal = 42
let negative_int = -17
let hex_literal = 0xFF
let octal_literal = 0o755
let binary_literal = 0b101010
let with_underscores = 1_000_000

(* Floating point literals *)
let float_literal = 3.14159
let scientific = 6.022e23
let negative_float = -2.71828

(* Character and string literals *)
let char_literal = 'a'
let escaped_char = '\n'
let unicode_char = '\u{03BB}'  (* λ *)
let string_literal = "Hello, OCaml!"
let multiline_string = {|
  This is a raw string
  that can span multiple lines
  without escape sequences
|}
let quoted_string = {delim|Contains "quotes" easily|delim}

(* Boolean literals *)
let bool_true = true
let bool_false = false

(* Unit value *)
let unit_value = ()

(* ============================================================================
   Type Definitions
   ============================================================================ *)

(* Type alias *)
type user_id = int
type email = string

(* Tuple types *)
type point = float * float
type rgb = int * int * int

(* Record types *)
type person = {
  id: int;
  name: string;
  age: int;
  mutable balance: float;  (* Mutable field *)
}

(* Record with optional field *)
type config = {
  host: string;
  port: int;
  timeout: int option;
}

(* Variant types (sum types) *)
type status =
  | Active
  | Inactive
  | Pending of string
  | Error of int * string

(* Polymorphic variants *)
type color = [ `Red | `Green | `Blue | `RGB of int * int * int ]

(* Recursive type *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(* Mutually recursive types *)
type expr =
  | Num of int
  | Var of string
  | BinOp of op * expr * expr
  | Let of string * expr * expr
  | Lambda of string * expr
  | App of expr * expr

and op =
  | Add
  | Sub
  | Mul
  | Div

(* Parameterized type *)
type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(* GADT-like using locally abstract types *)
type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value
  | Pair : 'a value * 'b value -> ('a * 'b) value

(* Extensible variant type *)
type shape = ..
type shape += Circle of float
type shape += Rectangle of float * float
type shape += Triangle of float * float * float

(* Private type *)
module PositiveInt : sig
  type t = private int
  val create : int -> t option
end = struct
  type t = int
  let create n = if n > 0 then Some n else None
end

(* ============================================================================
   Pattern Matching
   ============================================================================ *)

(* Simple pattern matching *)
let describe_status = function
  | Active -> "Currently active"
  | Inactive -> "Not active"
  | Pending reason -> "Pending: " ^ reason
  | Error (code, msg) -> Printf.sprintf "Error %d: %s" code msg

(* Pattern matching with guards *)
let classify_number n =
  match n with
  | 0 -> "zero"
  | n when n < 0 -> "negative"
  | n when n mod 2 = 0 -> "positive even"
  | _ -> "positive odd"

(* Pattern matching on lists *)
let rec sum_list = function
  | [] -> 0
  | x :: xs -> x + sum_list xs

let rec last_element = function
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last_element xs

(* Pattern matching with 'as' *)
let first_two = function
  | (x :: y :: _) as lst -> Some (x, y, lst)
  | _ -> None

(* Or-patterns *)
let is_weekend = function
  | "Saturday" | "Sunday" -> true
  | _ -> false

(* Exception pattern *)
let safe_divide x y =
  match y with
  | 0 -> None
  | _ -> Some (x / y)

(* Nested patterns *)
let deep_match = function
  | Node (x, Leaf, Leaf) -> `SingleNode x
  | Node (_, Node _, Leaf) -> `LeftOnly
  | Node (_, Leaf, Node _) -> `RightOnly
  | Node (_, Node _, Node _) -> `Both
  | Leaf -> `Empty

(* ============================================================================
   Functions
   ============================================================================ *)

(* Simple function *)
let add x y = x + y

(* Function with type annotation *)
let multiply (x : int) (y : int) : int = x * y

(* Recursive function *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

(* Tail-recursive function *)
let factorial_tail n =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (n * acc) (n - 1)
  in
  loop 1 n

(* Mutually recursive functions *)
let rec is_even n =
  if n = 0 then true else is_odd (n - 1)
and is_odd n =
  if n = 0 then false else is_even (n - 1)

(* Anonymous function (lambda) *)
let double = fun x -> x * 2

(* Labeled arguments *)
let create_person ~name ~age ~id =
  { id; name; age; balance = 0.0 }

(* Optional arguments with default *)
let greet ?(greeting = "Hello") name =
  Printf.printf "%s, %s!\n" greeting name

(* Optional arguments without default *)
let connect ~host ?port () =
  let p = match port with Some p -> p | None -> 80 in
  Printf.sprintf "%s:%d" host p

(* Function with local definitions *)
let complex_computation x =
  let square n = n * n in
  let cube n = n * n * n in
  let helper a b = square a + cube b in
  helper x (x + 1)

(* Partial application *)
let add_five = add 5

(* Function composition *)
let (>>) f g x = g (f x)
let process = String.trim >> String.lowercase_ascii >> String.capitalize_ascii

(* Operator definition *)
let ( <+> ) a b = a + b + 1
let ( |> ) x f = f x
let ( @@ ) f x = f x

(* ============================================================================
   Higher-Order Functions
   ============================================================================ *)

(* Map *)
let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs

(* Filter *)
let rec filter pred = function
  | [] -> []
  | x :: xs when pred x -> x :: filter pred xs
  | _ :: xs -> filter pred xs

(* Fold left *)
let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

(* Fold right *)
let rec fold_right f lst acc =
  match lst with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

(* Higher-order function examples *)
let sum = fold_left ( + ) 0
let product = fold_left ( * ) 1
let flatten = fold_right ( @ ) []

(* ============================================================================
   Modules and Functors
   ============================================================================ *)

(* Module definition *)
module IntSet = struct
  type t = int list

  let empty = []

  let add x s =
    if List.mem x s then s else x :: s

  let remove x = List.filter (fun y -> y <> x)

  let mem = List.mem

  let to_list s = List.sort compare s
end

(* Module type (signature) *)
module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module type SET = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val mem : elt -> t -> bool
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

(* Functor *)
module MakeSet (Ord : COMPARABLE) : SET with type elt = Ord.t = struct
  type elt = Ord.t
  type t = elt list

  let empty = []

  let rec add x = function
    | [] -> [x]
    | y :: ys as s ->
        match Ord.compare x y with
        | 0 -> s
        | n when n < 0 -> x :: s
        | _ -> y :: add x ys

  let rec remove x = function
    | [] -> []
    | y :: ys ->
        match Ord.compare x y with
        | 0 -> ys
        | n when n < 0 -> y :: ys
        | _ -> y :: remove x ys

  let rec mem x = function
    | [] -> false
    | y :: ys ->
        match Ord.compare x y with
        | 0 -> true
        | n when n < 0 -> false
        | _ -> mem x ys

  let fold f s acc = List.fold_right f s acc
end

(* Using the functor *)
module StringOrd = struct
  type t = string
  let compare = String.compare
end

module StringSet = MakeSet(StringOrd)

(* First-class modules *)
let choose_module (use_fast : bool) : (module COMPARABLE with type t = int) =
  if use_fast then
    (module struct
      type t = int
      let compare = compare
    end)
  else
    (module struct
      type t = int
      let compare x y = compare y x  (* reversed *)
    end)

(* Include and open *)
module ExtendedIntSet = struct
  include IntSet

  let size s = List.length s

  let union s1 s2 =
    fold_left (fun acc x -> add x acc) s1 s2
end

(* ============================================================================
   Object-Oriented Features
   ============================================================================ *)

(* Class definition *)
class point (x_init : float) (y_init : float) = object (self)
  val mutable x = x_init
  val mutable y = y_init

  method get_x = x
  method get_y = y

  method set_x new_x = x <- new_x
  method set_y new_y = y <- new_y

  method move dx dy =
    x <- x +. dx;
    y <- y +. dy

  method distance_to_origin =
    sqrt (x *. x +. y *. y)

  method to_string =
    Printf.sprintf "(%f, %f)" x y
end

(* Inheritance *)
class colored_point x y (c : string) = object
  inherit point x y as super

  val mutable color = c

  method get_color = color
  method set_color c = color <- c

  method! to_string =
    Printf.sprintf "%s[%s]" super#to_string color
end

(* Virtual class (abstract) *)
class virtual shape = object (self)
  method virtual area : float
  method virtual perimeter : float

  method describe =
    Printf.sprintf "Area: %f, Perimeter: %f" self#area self#perimeter
end

(* Implementing virtual class *)
class circle (r : float) = object
  inherit shape

  val radius = r

  method area = Float.pi *. radius *. radius
  method perimeter = 2.0 *. Float.pi *. radius
end

class rectangle (w : float) (h : float) = object
  inherit shape

  val width = w
  val height = h

  method area = width *. height
  method perimeter = 2.0 *. (width +. height)
end

(* Class type (interface) *)
class type printable = object
  method to_string : string
end

(* ============================================================================
   Imperative Features
   ============================================================================ *)

(* Mutable references *)
let counter = ref 0

let increment () =
  counter := !counter + 1;
  !counter

let reset () =
  counter := 0

(* Arrays *)
let numbers = [| 1; 2; 3; 4; 5 |]
let matrix = [| [| 1; 2 |]; [| 3; 4 |] |]

let array_operations () =
  let arr = Array.make 10 0 in
  for i = 0 to 9 do
    arr.(i) <- i * i
  done;
  arr

(* While loops *)
let find_first_even arr =
  let i = ref 0 in
  let found = ref None in
  while !i < Array.length arr && !found = None do
    if arr.(!i) mod 2 = 0 then
      found := Some arr.(!i);
    incr i
  done;
  !found

(* For loops *)
let print_array arr =
  for i = 0 to Array.length arr - 1 do
    Printf.printf "%d " arr.(i)
  done;
  print_newline ()

(* Hash tables *)
let create_lookup () =
  let tbl = Hashtbl.create 16 in
  Hashtbl.add tbl "one" 1;
  Hashtbl.add tbl "two" 2;
  Hashtbl.add tbl "three" 3;
  tbl

(* ============================================================================
   Exception Handling
   ============================================================================ *)

(* Define custom exception *)
exception Not_found_custom of string
exception Invalid_argument_custom of { name: string; value: string }

(* Raise exception *)
let find_element lst x =
  match List.find_opt ((=) x) lst with
  | Some v -> v
  | None -> raise (Not_found_custom (Printf.sprintf "Element %d not found" x))

(* Try-with *)
let safe_find lst x =
  try
    Some (find_element lst x)
  with
  | Not_found_custom msg ->
      Printf.eprintf "Warning: %s\n" msg;
      None
  | Invalid_argument _ ->
      None

(* Finally equivalent using Fun.protect *)
let with_file filename f =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> f ic)

(* Assert *)
let divide x y =
  assert (y <> 0);
  x / y

(* ============================================================================
   Monadic Patterns (without external libraries)
   ============================================================================ *)

(* Option monad *)
module OptionMonad = struct
  let return x = Some x

  let bind m f =
    match m with
    | None -> None
    | Some x -> f x

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) m f =
    match m with
    | None -> None
    | Some x -> Some (f x)

  let ( let+ ) = ( >>| )
end

(* Using option monad *)
let safe_div x y =
  if y = 0 then None else Some (x / y)

let computation a b c =
  let open OptionMonad in
  let* x = safe_div a b in
  let* y = safe_div x c in
  return (x + y)

(* Result monad *)
module ResultMonad = struct
  type ('a, 'e) t = Ok of 'a | Error of 'e

  let return x = Ok x

  let bind m f =
    match m with
    | Error e -> Error e
    | Ok x -> f x

  let ( >>= ) = bind
  let ( let* ) = bind

  let map f = function
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let ( >>| ) m f = map f m
  let ( let+ ) = ( >>| )
end

(* ============================================================================
   Lazy Evaluation
   ============================================================================ *)

(* Lazy values *)
let lazy_value = lazy (
  print_endline "Computing...";
  42
)

let force_lazy () =
  let result = Lazy.force lazy_value in
  Printf.printf "Result: %d\n" result

(* Infinite lazy list (stream) *)
type 'a stream = Cons of 'a * 'a stream Lazy.t

let rec naturals_from n : int stream =
  Cons (n, lazy (naturals_from (n + 1)))

let naturals = naturals_from 0

let rec take n (Cons (x, xs)) =
  if n <= 0 then []
  else x :: take (n - 1) (Lazy.force xs)

let rec map_stream f (Cons (x, xs)) =
  Cons (f x, lazy (map_stream f (Lazy.force xs)))

let rec filter_stream pred (Cons (x, xs)) =
  if pred x then
    Cons (x, lazy (filter_stream pred (Lazy.force xs)))
  else
    filter_stream pred (Lazy.force xs)

(* ============================================================================
   PPX and Attributes (syntax only - actual PPX requires preprocessing)
   ============================================================================ *)

(* Attributes *)
type user = {
  username: string [@key "user_name"];
  password: string [@key "pwd"];
} [@@deriving show, eq]

(* Extension points *)
(* let sql_query = [%sql "SELECT * FROM users WHERE id = ?"] *)

(* Alert/warning attributes *)
let old_function () [@alert deprecated "Use new_function instead"] =
  ()

(* Inline attribute *)
let[@inline always] fast_add x y = x + y

(* ============================================================================
   Main Entry Point
   ============================================================================ *)

let () =
  print_endline "=== OCaml Sample Program ===";

  (* Basic operations *)
  Printf.printf "Factorial 10: %d\n" (factorial 10);
  Printf.printf "Sum [1..10]: %d\n" (sum [1;2;3;4;5;6;7;8;9;10]);

  (* Pattern matching *)
  print_endline (describe_status Active);
  print_endline (describe_status (Pending "approval"));

  (* Records *)
  let alice = create_person ~name:"Alice" ~age:30 ~id:1 in
  Printf.printf "Person: %s, age %d\n" alice.name alice.age;

  (* Modules *)
  let set = IntSet.(empty |> add 1 |> add 2 |> add 3) in
  Printf.printf "Set: [%s]\n"
    (String.concat "; " (List.map string_of_int (IntSet.to_list set)));

  (* Objects *)
  let p = new colored_point 3.0 4.0 "red" in
  Printf.printf "Point: %s, distance: %f\n" p#to_string p#distance_to_origin;

  (* Lazy evaluation *)
  Printf.printf "First 10 naturals: [%s]\n"
    (String.concat "; " (List.map string_of_int (take 10 naturals)));

  print_endline "Done!"
