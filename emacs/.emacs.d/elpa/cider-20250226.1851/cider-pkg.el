;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "cider" "20250226.1851"
  "Clojure Interactive Development Environment that Rocks."
  '((emacs        "26")
    (clojure-mode "5.19")
    (parseedn     "1.2.1")
    (queue        "0.2")
    (spinner      "1.7")
    (seq          "2.22")
    (sesman       "0.3.2")
    (transient    "0.4.1"))
  :url "https://www.github.com/clojure-emacs/cider"
  :commit "531c91e81d12484fc0120b77c9fb0a451cd8c2de"
  :revdesc "531c91e81d12"
  :keywords '("languages" "clojure" "cider")
  :authors '(("Tim King" . "kingtim@gmail.com")
             ("Phil Hagelberg" . "technomancy@gmail.com")
             ("Bozhidar Batsov" . "bozhidar@batsov.dev")
             ("Artur Malabarba" . "bruce.connor.am@gmail.com")
             ("Hugo Duncan" . "hugo@hugoduncan.org")
             ("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
