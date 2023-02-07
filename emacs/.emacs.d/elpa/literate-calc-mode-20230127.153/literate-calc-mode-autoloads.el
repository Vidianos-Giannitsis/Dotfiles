;;; literate-calc-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "literate-calc-mode" "literate-calc-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from literate-calc-mode.el

(autoload 'literate-calc-clear-overlays "literate-calc-mode" "\
Remove all literate-calc-mode overlays in the current buffer." t nil)

(autoload 'literate-calc-eval-line "literate-calc-mode" "\
Evaluate the calc expression on the current line." t nil)

(autoload 'literate-calc-eval-buffer "literate-calc-mode" "\
Evaluate all calc expressions in the current buffer in order.

If BUFFER is set, run in it, otherwise in `current-buffer'.

\(fn &optional (BUFFER (current-buffer)))" t nil)

(autoload 'literate-calc-insert-results "literate-calc-mode" "\
Insert results into buffer instead of creating overlays." t nil)

(autoload 'literate-calc-remove-results "literate-calc-mode" "\
Remove inserted results from buffer between START and END.

\(fn START END)" t nil)

(autoload 'literate-calc-mode "literate-calc-mode" "\


\(fn)" t nil)

(autoload 'literate-calc-minor-mode "literate-calc-mode" "\
Evaluates calc expressions

This is a minor mode.  If called interactively, toggle the
`Literate-Calc minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `literate-calc-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "literate-calc-mode" '("literate-calc-" "org-babel-ex"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; literate-calc-mode-autoloads.el ends here
