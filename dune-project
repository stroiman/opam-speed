(lang dune 3.15)

(name speed)

(generate_opam_files true)

(source
 (github stroiman/opam-speed))

(authors "Peter Strøiman")

(maintainers "Peter Strøiman")

(license APACHE)

; (documentation https://url/to/documentation)

(package
 (name speed)
 (synopsis "Speed - go fast by writing test first")
 (description "Test framework inspired by the greatest I've yet worked with, RSpec")
 (depends ocaml dune ocolor ppxlib base lwt lwt_ppx)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
