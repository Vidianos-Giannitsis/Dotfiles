;;; lispyville-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lispyville" "lispyville.el" (0 0 0 0))
;;; Generated autoloads from lispyville.el

(autoload 'lispyville-mode "lispyville" "\
A minor mode for integrating evil with lispy.

This is a minor mode.  If called interactively, toggle the
`Lispyville mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lispyville-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lispyville-set-key-theme "lispyville" "\
Binds keys in lispyville-mode-map according to THEME.
When THEME is not given, `lispyville-key-theme' will be used instead.

\(fn &optional THEME)" nil nil)

(register-definition-prefixes "lispyville" '("lispyville-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lispyville-autoloads.el ends here
