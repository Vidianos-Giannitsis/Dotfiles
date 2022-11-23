;;; oer-reveal-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "oer-reveal" "oer-reveal.el" (0 0 0 0))
;;; Generated autoloads from oer-reveal.el

(autoload 'oer-reveal-export-attribution "oer-reveal" "\
Generate HTML and LaTeX code for image with license attribution.
Essentially, this function calls `oer-reveal--export-attribution-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2.

\(fn &rest ARGS)" nil nil)

(autoload 'oer-reveal-export-image-grid "oer-reveal" "\
Generate HTML for image grid.
Essentially, this function calls `oer-reveal--export-image-grid-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2.

\(fn &rest ARGS)" nil nil)

(autoload 'oer-reveal-publish-to-reveal "oer-reveal" "\
Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(autoload 'oer-reveal-publish-to-reveal-and-pdf "oer-reveal" "\
Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(autoload 'oer-reveal-publish-to-reveal-client "oer-reveal" "\
Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
If `org-re-reveal-client-multiplex-filter' is a regular expression (not
nil), only publish FILENAME if it matches this regular expression.
Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(register-definition-prefixes "oer-reveal" '("oer-reveal-"))

;;;***

;;;### (autoloads nil "oer-reveal-publish" "oer-reveal-publish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from oer-reveal-publish.el

(autoload 'oer-reveal-publish-setq-defaults "oer-reveal-publish" "\
Change Emacs environment.
Load babel languages in `oer-reveal-publish-babel-languages',
add `oer-reveal-publish-alternate-type-function' to
`org-export-before-processing-hook'.
Also fix internal slide references by assignment to
`org-re-reveal--href-fragment-prefix', see its doc string.  Note that
this change needs `setq' before `org-re-reveal-ref' is loaded, as
that library again uses `setq'.  Also, using `oer-reveal--add-advice-link'
during publication needs the proper value." nil nil)

(register-definition-prefixes "oer-reveal-publish" '("oer-reveal-publish-"))

;;;***

;;;### (autoloads nil nil ("oer-reveal-pkg.el" "ox-oer-reveal.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; oer-reveal-autoloads.el ends here
