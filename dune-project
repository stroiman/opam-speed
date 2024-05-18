(lang dune 3.15)

(name speed)

(generate_opam_files true)

(source
 (github stroiman/opam-speed))

(authors "Peter Strøiman")

(maintainers "Peter Strøiman")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name speed)
 (synopsis "Speed - go fast by writing test first")
 (description 
"Speed is a test framework encouraging a fast feedback loop by writing tests 
first. This is inspired by other tools bourn out of the BDD philosophy. It
draws inspiration from the JavaScript libraries, mocha/jasmine/jest, as well as 
the Ruby library RSpec")
 (depends (ocaml (>= 5)) dune ocolor ppxlib lwt lwt_ppx (ppx_jane :with-test))
 (tags
  (tdd testing "unit-testing" bdd)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
