;;; elfeed-summary-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elfeed-summary" "elfeed-summary.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from elfeed-summary.el

(autoload 'elfeed-summary "elfeed-summary" "\
Display a feed summary for elfeed.

The feed summary is a tree of three basic items: groups, feeds and
searches.  Groups also may contain other items.  The structure of the
tree is determined by the `elfeed-summary-settings' variable.

Take a look at `elfeed-summary-mode' for the list of available
keybindings, and at the `elfeed-summary' group for the available
options." t nil)

(register-definition-prefixes "elfeed-summary" '("elfeed-summary-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elfeed-summary-autoloads.el ends here
