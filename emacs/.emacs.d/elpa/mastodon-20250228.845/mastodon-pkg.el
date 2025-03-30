;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "mastodon" "20250228.845"
  "Client for fediverse services using the Mastodon API."
  '((emacs   "28.1")
    (request "0.3.0")
    (persist "0.4")
    (tp      "0.7"))
  :url "https://codeberg.org/martianh/mastodon.el"
  :commit "9561697f7b6238e542f1d244449b3cd515bdd150"
  :revdesc "9561697f7b62"
  :authors '(("Johnson Denen" . "johnson.denen@gmail.com")
             ("Marty Hiatt" . "mousebot@disroot.org"))
  :maintainers '(("Marty Hiatt" . "mousebot@disroot.org")))
