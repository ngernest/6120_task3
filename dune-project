(lang dune 3.16)

(name bril_getting_started)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name bril_getting_started)
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