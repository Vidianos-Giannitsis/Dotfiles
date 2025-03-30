;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250218.1722"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "0037daaf3eb2d1c3a1c215efb4d38a32db140224"
  :revdesc "0037daaf3eb2"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
