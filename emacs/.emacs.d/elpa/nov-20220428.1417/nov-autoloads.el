;;; nov-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nov" "nov.el" (0 0 0 0))
;;; Generated autoloads from nov.el

(autoload 'nov-mode "nov" "\
Major mode for reading EPUB documents

\(fn)" t nil)

(autoload 'nov-bookmark-jump-handler "nov" "\
The bookmark handler-function interface for bookmark BMK.

See also `nov-bookmark-make-record'.

\(fn BMK)" nil nil)

(register-definition-prefixes "nov" '("nov-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nov-autoloads.el ends here
