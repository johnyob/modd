(lang dune 3.7)

(name modd)

(generate_opam_files true)

(source
 (github johnyob/modd))

(authors "Alistair O'Brien <ajob410@gmail.com>")

(maintainers "Alistair O'Brien")

(license MIT)

(using menhir 2.1)

(package
 (name modd)
 (synopsis
  "Modd is a minimal ML with modes using constraint-based type inference!")
 (depends ocaml dune core ppx_jane menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
