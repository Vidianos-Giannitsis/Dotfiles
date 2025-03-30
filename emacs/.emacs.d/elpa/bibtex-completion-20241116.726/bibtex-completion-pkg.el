;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "bibtex-completion" "20241116.726"
  "A BibTeX backend for completion frameworks."
  '((parsebib "6.0")
    (s        "1.9.0")
    (dash     "2.6.0")
    (f        "0.16.2")
    (cl-lib   "0.5")
    (biblio   "0.2")
    (emacs    "26.1"))
  :url "https://github.com/tmalsburg/helm-bibtex"
  :commit "6064e8625b2958f34d6d40312903a85c173b5261"
  :revdesc "6064e8625b29"
  :authors '(("Titus von der Malsburg" . "malsburg@posteo.de")
             ("Justin Burkett" . "justin@burkett.cc"))
  :maintainers '(("Titus von der Malsburg" . "malsburg@posteo.de")))
