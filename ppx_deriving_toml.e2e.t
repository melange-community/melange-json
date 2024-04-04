
  $ echo '(lang dune 3.11)' > dune-project

  $ echo '
  > (executable 
  >   (name tuple)
  >   (modules tuple)
  >   (libraries toml)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name record)
  >   (modules record)
  >   (libraries toml)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name variant)
  >   (modules variant)
  >   (libraries toml)
  >   (preprocess (pps ppx_deriving_toml)))
  > 
  > (executable 
  >   (name variant_record)
  >   (modules variant_record)
  >   (libraries toml)
  >   (preprocess (pps ppx_deriving_toml)))
  > ' > dune

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type t = { a : string; b : int } [@@deriving toml]
  > let t = { a = "foo"; b = 42 }
  > let t_toml = Otoml.Printer.to_string (to_toml t)
  > let () = print_endline t_toml
  > let t0 = of_toml (Otoml.Parser.from_string t_toml)
  > let () = assert (t = t0)
  > ' >> record.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type t = string * int [@@deriving toml]
  > type top = {top : t} [@@deriving toml]
  > let t = {top = ("foo", 42)}
  > let t_toml = Otoml.Printer.to_string (top_to_toml t)
  > let () = print_endline t_toml
  > let t0 = top_of_toml (Otoml.Parser.from_string t_toml)
  > let () = assert (t = t0)
  > ' >> tuple.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type t = A of string | B of int [@@deriving toml]
  > type top = {top : t} [@@deriving toml]
  > let t = {top = A "foo"}
  > let t_toml = Otoml.Printer.to_string (top_to_toml t)
  > let () = print_endline t_toml
  > let t0 = top_of_toml (Otoml.Parser.from_string t_toml)
  > let () = assert (t = t0)
  > ' >> variant.ml

  $ echo '
  > open Ppx_deriving_toml_runtime
  > type t = A of {a: string} | B of {b: int} [@@deriving toml]
  > type top = {top : t} [@@deriving toml]
  > let t = {top = A {a="foo"}}
  > let t_toml = Otoml.Printer.to_string (top_to_toml t)
  > let () = print_endline t_toml
  > let t0 = top_of_toml (Otoml.Parser.from_string t_toml)
  > let () = assert (t = t0)
  > ' >> variant_record.ml

  $ dune exec ./record.exe
  a = "foo"
  b = 42
  

  $ dune exec ./tuple.exe
  top = ["foo", 42]
  

  $ dune exec ./variant.exe
  top = {type = "A", args = ["foo"]}
  

  $ dune exec ./variant_record.exe
  
  [top]
    type = "A"
    a = "foo"
  
