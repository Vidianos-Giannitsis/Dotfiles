;;; python-mls-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-mls" "python-mls.el" (0 0 0 0))
;;; Generated autoloads from python-mls.el

(autoload 'python-mls-check-prompt "python-mls" "\
Check for prompt, after input is sent.
If a continuation prompt is found in OUTPUT, fix up comint to
handle it.  Multi-line statements are handled directly.  If a
single command sent to (i)Python is the start of multi-line
statment, the process will return a continuation prompt.  Remove
it, sanitize the history, and then bring the last input forward
to continue.  Run the hook `python-mls-after-prompt-hook' in idle
time after a normal prompt is detected.

\(fn OUTPUT)" nil nil)

(autoload 'python-mls-setup "python-mls" "\
Enable python-mls for python shells and buffers.
If DISABLE is non-nil, disable instead.

\(fn &optional DISABLE)" t nil)

(autoload 'python-mls-mode "python-mls" "\
Minor mode enabling multi-line statements in inferior (i)Python buffers.

If called interactively, enable Python-Mls mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-mls" '("python-mls-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-mls-autoloads.el ends here
