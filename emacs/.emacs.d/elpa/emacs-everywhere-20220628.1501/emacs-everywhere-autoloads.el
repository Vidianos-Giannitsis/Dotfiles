;;; emacs-everywhere-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emacs-everywhere" "emacs-everywhere.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emacs-everywhere.el

(autoload 'emacs-everywhere "emacs-everywhere" "\
Lanuch the emacs-everywhere frame from emacsclient.

\(fn &optional FILE LINE COLUMN)" nil nil)

(autoload 'emacs-everywhere-initialise "emacs-everywhere" "\
Entry point for the executable.
APP is an `emacs-everywhere-app' struct." nil nil)

(add-hook 'server-visit-hook #'emacs-everywhere-initialise)

(register-definition-prefixes "emacs-everywhere" '("emacs-everywhere-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacs-everywhere-autoloads.el ends here
