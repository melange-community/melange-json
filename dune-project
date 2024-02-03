(lang dune 3.9)

(using melange 0.1)

(name melange-json)

(generate_opam_files true)

(authors "glennsl")

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
 (synopsis "Compositional JSON encode/decode library for Melange")
 (description
  "Provides encoders and decoders to convert JSON values into typed values. With the possibility to create custom encoders and decoders.")
 (depends
  (ocaml
   (>= "5.1"))
  (melange
   (>= "3.0.0"))
  (melange-jest :with-test)
  (reason
   (and
    (>= "3.10.0")
    :with-test))
  (opam-check-npm-deps :with-test) ; todo: use with-dev-setup once opam 2.2 is out
  (ocaml-lsp-server :with-test)))
