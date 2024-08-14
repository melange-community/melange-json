(lang dune 3.9)

(using melange 0.1)

(name melange-json)

(generate_opam_files true)

(authors "glennsl" "Andrey Popp")

(maintainers
 "Antonio Nuno Monteiro <anmonteiro@gmail.com>"
 "Javier Ch\195\161varri <javier.chavarri@gmail.com>")

(license "LGPL-3.0-only" "MPL-2.0")

(homepage "https://github.com/melange-community/melange-json/")

(bug_reports "https://github.com/melange-community/melange-json/issues")

(source
 (github melange-community/melange-json))

(package
 (name melange-json)
 (synopsis "Compositional JSON encode/decode library and PPX for Melange, with native compatibility")
 (description
  "Provides encoders and decoders to convert JSON values into typed values. With the possibility to create custom encoders and decoders and automate them with a PPX.")
 (depends
  ocaml
  (melange
   (>= "3.0.0"))
  (melange-jest :with-test)
  (reason
   (and
    (>= "3.10.0")
    :with-test))
  ppxlib
  containers
  yojson ; only used for the native version
  (opam-check-npm-deps :with-test) ; todo: use with-dev-setup once opam 2.2 is out
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)))
