;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20241221.1420"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.3.0")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "50d42b1395d6381fef66ff8aae4b0d171f7e5b36"
  :revdesc "50d42b1395d6"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
