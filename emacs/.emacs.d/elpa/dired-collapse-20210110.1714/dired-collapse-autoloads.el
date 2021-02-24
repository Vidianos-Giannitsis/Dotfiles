;;; dired-collapse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-collapse" "dired-collapse.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dired-collapse.el

(autoload 'dired-collapse-mode "dired-collapse" "\
Toggle collapsing of unique nested paths in Dired.

If called interactively, enable Dired-Collapse mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-collapse" '("dired-collapse")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-collapse-autoloads.el ends here
