(lang dune 3.9)

(using melange 0.1)

(name melange-json)

(generate_opam_files true)

(maintainers
 "Antonio Nuno Monteiro <anmonteiro@gmail.com>"
 "Javier Chávarri <javier.chavarri@gmail.com>")

(license "LGPL-3.0-only" "MPL-2.0")

(homepage "https://github.com/melange-community/melange-json/")

(bug_reports "https://github.com/melange-community/melange-json/issues")

(source
 (github melange-community/melange-json))

(package
 (name melange-json)
 (authors "glennsl" "Andrey Popp")
 (synopsis "Compositional JSON encode/decode library and PPX for Melange")
 (description
  "Provides tools for converting JSON to typed OCaml values in Melange. It includes custom encoders, decoders, and a PPX for automating these conversions.")
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
  (opam-check-npm-deps :with-test) ; todo: use with-dev-setup once opam 2.2 is out
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)))

(package
 (name melange-json-native)
 (authors "Andrey Popp")
 (synopsis "Compositional JSON encode/decode PPX for OCaml")
 (description
  "A PPX for OCaml that automates encoding and decoding JSON into typed values. It supports custom encoders and decoders, and integrates with Yojson")
 (depends
  (ocaml
   (>= "4.12"))
  (ppxlib
   (>= "0.29.0"))
  (yojson
   (>= "1.6.0"))))
