;;; dired-collapse-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-collapse" "dired-collapse.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dired-collapse.el

(autoload 'dired-collapse-mode "dired-collapse" "\
Toggle collapsing of unique nested paths in Dired.

This is a minor mode.  If called interactively, toggle the
`Dired-Collapse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dired-collapse-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dired-collapse" '("dired-collapse"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-collapse-autoloads.el ends here
