;;; lispyville-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lispyville" "lispyville.el" (0 0 0 0))
;;; Generated autoloads from lispyville.el

(autoload 'lispyville-mode "lispyville" "\
A minor mode for integrating evil with lispy.

If called interactively, enable Lispyville mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lispyville-set-key-theme "lispyville" "\
Binds keys in lispyville-mode-map according to THEME.
When THEME is not given, `lispyville-key-theme' will be used instead.

\(fn &optional THEME)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lispyville" '("lispyville-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lispyville-autoloads.el ends here
