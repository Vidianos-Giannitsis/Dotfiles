;;; devdocs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "devdocs" "devdocs.el" (0 0 0 0))
;;; Generated autoloads from devdocs.el

(autoload 'devdocs-delete "devdocs" "\
Delete DevDocs documentation.
DOC is a document slug.

\(fn DOC)" t nil)

(autoload 'devdocs-install "devdocs" "\
Download and install DevDocs documentation.
DOC is a document slug.

\(fn DOC)" t nil)

(autoload 'devdocs-lookup "devdocs" "\
Look up a DevDocs documentation entry.

Display entries in the documents `devdocs-current-docs' for
selection.  With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read a list of available documents and set
`devdocs-current-docs' for this buffer.

\(fn &optional ASK-DOCS)" t nil)

(autoload 'devdocs-search "devdocs" "\
Search for QUERY in the DevDocs website.

\(fn QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "devdocs" '("devdocs-")))

;;;***

;;;### (autoloads nil nil ("devdocs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; devdocs-autoloads.el ends here
