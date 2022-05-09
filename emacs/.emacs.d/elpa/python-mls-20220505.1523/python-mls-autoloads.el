;;; python-mls-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-mls" "python-mls.el" (0 0 0 0))
;;; Generated autoloads from python-mls.el

(autoload 'python-mls-check-prompt "python-mls" "\
Check for prompt, after input is sent.
If a continuation prompt is found in the buffer, fix up comint to
handle it.  Multi-line statements are handled directly.  If a
single command sent to (i)Python is the start of multi-line
statment, the process will return a continuation prompt.  Remove
it, sanitize the history, and then bring the last input forward
to continue.  Run the hook `python-mls-after-prompt-hook' after a
normal prompt is detected.

\(fn PROCESS OUTPUT &rest _)" nil nil)

(autoload 'python-mls-setup "python-mls" "\
Enable python-mls for python shells and buffers.
If DISABLE is non-nil, disable instead.

\(fn &optional DISABLE)" t nil)

(autoload 'python-mls-mode "python-mls" "\
Minor mode enabling multi-line statements in inferior (i)Python buffers.

This is a minor mode.  If called interactively, toggle the
`Python-Mls mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `python-mls-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "python-mls" '("python-mls-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-mls-autoloads.el ends here