(lang dune 3.10)

(using melange 0.1)

(generate_opam_files true)

(source
 (github andreypopp/ppx_deriving))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(package
 (name ppx_deriving_json)
 (depends
  (ocaml
   (>= 4.14))
  melange
  dune
  ppxlib
  containers
  yojson
  (ocamlformat :with-test)))

(package
 (name ppx_deriving_toml)
 (depends
  (ocaml
   (>= 4.14))
  dune
  ppxlib
  otoml
  containers))
