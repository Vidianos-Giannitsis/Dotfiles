;;; all-the-icons-ibuffer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "all-the-icons-ibuffer" "all-the-icons-ibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from all-the-icons-ibuffer.el

(autoload 'all-the-icons-ibuffer-mode "all-the-icons-ibuffer" "\
Display icons for all buffers in ibuffer.

This is a minor mode.  If called interactively, toggle the
`All-The-Icons-Ibuffer mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `all-the-icons-ibuffer-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "all-the-icons-ibuffer" '("all-the-icons-ibuffer-" "filename-and-process+" "icon" "mode+" "size-h"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-the-icons-ibuffer-autoloads.el ends here
