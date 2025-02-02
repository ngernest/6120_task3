(lang dune 3.16)

(name 6120_task3)

(generate_opam_files true)

(source
 (github ngernest/6120_task3))

(authors "Author Name")

(license LICENSE)

(package
 (name 6120_task3)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ppx_quick_test
   (>= v0.17.0))
  (base_quickcheck
   (>= v0.17.0))
  (yojson
   (>= 2.2.2))
  (ppx_jane
   (>= v0.17.0))
  (core
   (>= v0.17.1))
  ocaml
  dune)
 (tags
  (topics "to describe" your project)))