(define-package "magit" "20220926.56" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (compat "28.1.1.2")
    (dash "20210826")
    (git-commit "20220222")
    (magit-section "20220325")
    (transient "20220325")
    (with-editor "20220318"))
  :commit "ada7b21fcc90004d7ac1c5a42c0750e12c2a5ef8" :authors
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
