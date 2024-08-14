
  $ echo '(lang dune 3.11)' > dune-project

  $ echo '
  > (library 
  >   (name harness)
  >   (modules harness)
  >   (libraries otoml)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name tuple)
  >   (modules tuple)
  >   (libraries otoml harness)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name record)
  >   (modules record)
  >   (libraries otoml harness)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name variant)
  >   (modules variant)
  >   (libraries otoml harness)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name variant_record)
  >   (modules variant_record)
  >   (libraries otoml harness)
  >   (preprocess (pps ppx_deriving_toml)))
  > ' > dune

  $ echo "
  > type 'a t = {top : 'a} [@@deriving toml]
  > 
  > let decode ~v_of_toml toml =
  >   (of_toml v_of_toml (Otoml.Parser.from_string toml)).top
  > 
  > let reprint ?msg ~v_to_toml ~v_of_toml v =
  >   Option.iter print_endline msg;
  >   let t = { top = v } in
  >   let t_toml = Otoml.Printer.to_string (to_toml v_to_toml t) in
  >   print_endline t_toml;
  >   assert (v = decode ~v_of_toml t_toml)
  > 
  > let decode_then_reprint ?msg ~v_to_toml ~v_of_toml toml =
  >   reprint ?msg ~v_to_toml ~v_of_toml @@ decode ~v_of_toml toml
  > " > harness.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type v = { 
  >   a : string;
  >   b : int; [@toml.key "B"]
  >   c : bool; [@toml.default false]
  >   e : int option; [@toml.option]
  > } [@@deriving toml]
  > let v = { a = "foo"; b = 42; c = true; e = None; };;
  > Harness.reprint ~msg:"reprint:" v ~v_to_toml ~v_of_toml;;
  > Harness.decode_then_reprint ~msg:"decode:" ~v_to_toml ~v_of_toml {|
  > [top]
  > a = "a"
  > B = 1
  > |};;
  > Harness.decode_then_reprint ~msg:"decode with optionals:" ~v_to_toml ~v_of_toml {|
  > [top]
  > a = "a"
  > B = 1
  > c = true
  > e = 42
  > |};;
  > ' >> record.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type v = string * int [@@deriving toml]
  > let v = ("foo", 42)
  > let () = Harness.reprint v ~v_to_toml ~v_of_toml
  > ' >> tuple.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type v = A of string | B of int [@@deriving toml]
  > let v = A "foo"
  > let () = Harness.reprint v ~v_to_toml ~v_of_toml
  > ' >> variant.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type v = A of {a: string} | B of {b: int} [@@deriving toml]
  > let v = A {a = "foo"}
  > let () = Harness.reprint v ~v_to_toml ~v_of_toml
  > ' >> variant_record.ml

  $ dune exec ./record.exe
  reprint:
  
  [top]
    a = "foo"
    B = 42
    c = true
  
  decode:
  
  [top]
    a = "a"
    B = 1
    c = false
  
  decode with optionals:
  
  [top]
    a = "a"
    B = 1
    c = true
    e = 42
  

  $ dune exec ./tuple.exe
  top = ["foo", 42]
  

  $ dune exec ./variant.exe
  top = {type = "A", args = ["foo"]}
  

  $ dune exec ./variant_record.exe
  
  [top]
    type = "A"
    a = "foo"
  
