;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "citeproc" "20240722.1110"
  "A CSL 1.0.2 Citation Processor."
  '((emacs             "26")
    (dash              "2.13.0")
    (s                 "1.12.0")
    (f                 "0.18.0")
    (queue             "0.2")
    (string-inflection "1.0")
    (org               "9")
    (parsebib          "2.4")
    (compat            "28.1"))
  :url "https://github.com/andras-simonyi/citeproc-el"
  :commit "54184baaff555b5c7993d566d75dd04ed485b5c0"
  :revdesc "54184baaff55"
  :keywords '("bib")
  :authors '(("András Simonyi" . "andras.simonyi@gmail.com"))
  :maintainers '(("András Simonyi" . "andras.simonyi@gmail.com")))
