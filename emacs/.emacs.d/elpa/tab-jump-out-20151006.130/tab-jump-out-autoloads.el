;;; tab-jump-out-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tab-jump-out" "tab-jump-out.el" (0 0 0 0))
;;; Generated autoloads from tab-jump-out.el

(autoload 'tab-jump-out "tab-jump-out" "\
Use tab to jump out.

\(fn ARG)" t nil)

(autoload 'tab-jump-out-mode "tab-jump-out" "\
A minor mode that allows you to jump out with tab.

If called interactively, enable Tab-Jump-Out mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tab-jump-out" '("tab-jump-out-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tab-jump-out-autoloads.el ends here
