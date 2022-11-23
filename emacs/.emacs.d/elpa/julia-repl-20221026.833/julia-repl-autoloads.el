;;; julia-repl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-repl" "julia-repl.el" (0 0 0 0))
;;; Generated autoloads from julia-repl.el

(autoload 'julia-repl "julia-repl" "\
Raise the Julia REPL inferior buffer, creating one if it does not exist.

This is the standard entry point for using this package." t nil)

(autoload 'julia-repl-mode "julia-repl" "\
Minor mode for interacting with a Julia REPL running inside a term.

This is a minor mode.  If called interactively, toggle the
`Julia-Repl mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `julia-repl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{julia-repl-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "julia-repl" '("julia-repl-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-repl-autoloads.el ends here
