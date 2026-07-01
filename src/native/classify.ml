type t = Yojson.Safe.t

(* The public [classify]/[declassify] view is the 7-constructor JSON model
   shared with the Melange (browser) backend. [Yojson.Safe.t] additionally
   has [`Intlit], [`Tuple] and [`Variant], so we normalize them here:

   - [`Intlit] carries an integer literal that did not fit a native [int]
     during parsing. We surface it as an [`Int] when it does fit (defensive)
     and otherwise as a [`Float], matching how a JS runtime represents an
     out-of-range integer.
   - [`Tuple] and [`Variant] only arise from yojson's non-standard syntax
     extensions (never from [from_string] on plain JSON); we still map them
     to the closest standard shape so [classify] stays total. *)

let classify :
    t ->
    [ `Null
    | `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `List of t list
    | `Assoc of (string * t) list ] = function
  | `Null -> `Null
  | `String s -> `String s
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `Intlit s -> (
      match int_of_string_opt s with
      | Some i -> `Int i
      | None -> `Float (float_of_string s))
  | `Bool b -> `Bool b
  | `List l -> `List l
  | `Tuple l -> `List l
  | `Assoc a -> `Assoc a
  | `Variant (name, None) -> `String name
  | `Variant (name, Some v) -> `List [ `String name; v ]

let declassify :
    [ `Null
    | `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `List of t list
    | `Assoc of (string * t) list ] ->
    t =
 fun x -> (x :> t)
