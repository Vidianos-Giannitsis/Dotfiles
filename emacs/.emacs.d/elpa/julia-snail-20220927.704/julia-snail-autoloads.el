;;; julia-snail-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-snail" "julia-snail.el" (0 0 0 0))
;;; Generated autoloads from julia-snail.el

(autoload 'julia-snail "julia-snail" "\
Start a Julia REPL and connect to it, or switch if one already exists.
The following buffer-local variables control it:
- `julia-snail-repl-buffer' (default: *julia*)
- `julia-snail-port' (default: 10011)
To create multiple REPLs, give these variables distinct values (e.g.:
*julia my-project-1* and 10012)." t nil)

(autoload 'julia-snail-mode "julia-snail" "\
A minor mode for interactive Julia development.
Should only be turned on in source buffers.

This is a minor mode.  If called interactively, toggle the
`Julia-Snail mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `julia-snail-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following keys are set:
\\{julia-snail-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'julia-snail-repl-mode "julia-snail" "\
A minor mode for interactive Julia development.
Should only be turned on in REPL buffers.

This is a minor mode.  If called interactively, toggle the
`Julia-Snail-Repl mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `julia-snail-repl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following keys are set:
\\{julia-snail-repl-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "julia-snail" '("julia-snail-"))

;;;***

;;;### (autoloads nil nil ("julia-snail-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-snail-autoloads.el ends here
