(define-package "magit" "20220120.2330" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "20210826")
    (git-commit "20211004")
    (magit-section "20211004")
    (transient "20210920")
    (with-editor "20211001"))
  :commit "ed7a4b24a2fb7e18259caed7a469998b2d2aac85" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
