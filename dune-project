(lang dune 3.0)

(name ocaml-quint-connect)

(generate_opam_files true)

(source
 (github mathiasbourgoin/ocaml-quint-connect))

(license MIT)

(authors "Mathias Bourgoin <mathias.bourgoin@gmail.com>")

(maintainers "Mathias Bourgoin <mathias.bourgoin@gmail.com>")

(package
 (name ocaml-quint-connect)
 (synopsis "Parse Quint ITF traces and replay them against OCaml implementations")
 (description
  "Parses Quint ITF (Informal Trace Format) traces, including model-based-testing metadata (action taken, nondeterministic picks), replays them step by step against an OCaml implementation through a driver, and compares observable state with the model. Includes PPX helpers and a quint-connect CLI.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  yojson
  (ppxlib
   (>= 0.26))
  (alcotest :with-test)
  (sqlite3 :with-test)))
