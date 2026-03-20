(lang dune 2.1)
(name sparrow)
(generate_opam_files true)

(package
 (name sparrow)
 (synopsis "Static analyzer for C")
 (description "Sparrow is a state-of-the-art static analyzer that aims to verify the absence of fatal bugs in C source.")
 (depends
  (ocaml (>= 4.04.0))
  (dune (>= 2.1))
  batteries
  ocamlgraph
  apron
  yojson
  pyml
  ppx_compare
  ppx_deriving
  goblint-cil))
