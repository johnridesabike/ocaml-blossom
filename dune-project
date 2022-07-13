(lang dune 3.3)

(name blossom)

(generate_opam_files true)

(source
 (github johnridesabike/ocaml-blossom))

(authors "John Jackson")

(maintainers "John Jackson")

(license MPL-2.0)

(package
 (name blossom)
 (synopsis "Maximum weighted matching on undirected graphs")
 (depends ocaml dune (alcotest :with-test))
 (tags
  ("blossom algorithm" matching "maximum weighted matching")))
