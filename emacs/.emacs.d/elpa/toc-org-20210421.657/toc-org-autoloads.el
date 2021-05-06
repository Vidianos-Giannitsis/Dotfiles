;;; toc-org-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "toc-org" "toc-org.el" (0 0 0 0))
;;; Generated autoloads from toc-org.el

(autoload 'toc-org-enable "toc-org" "\
Enable toc-org in this buffer." nil nil)

(autoload 'toc-org-mode "toc-org" "\
Toggle `toc-org' in this buffer.

If called interactively, enable Toc-Org mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toc-org" '("toc-org-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; toc-org-autoloads.el ends here
