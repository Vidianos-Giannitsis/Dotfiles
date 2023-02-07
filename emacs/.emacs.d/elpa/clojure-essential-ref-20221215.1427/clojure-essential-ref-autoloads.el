;;; clojure-essential-ref-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clojure-essential-ref" "clojure-essential-ref.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from clojure-essential-ref.el

(autoload 'clojure-essential-ref "clojure-essential-ref" "\
Open Clojure documentation for symbol.

Book \"Clojure, The Essential Reference\" is used as a documentation source.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(autoload 'clojure-essential-ref-web "clojure-essential-ref" "\
Open Clojure documentation for symbol in the default web browser.

Online version of book \"Clojure, The Essential Reference\" is
used as a documentation source.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "clojure-essential-ref" '("clojure-essential-ref-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-essential-ref-autoloads.el ends here
