;;; python-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-mode" "python-mode.el" (0 0 0 0))
;;; Generated autoloads from python-mode.el

(autoload 'py-backward-class-bol "python-mode" "\
Go to beginning of `class', go to BOL.
If already at beginning, go one `class' backward.
Return beginning of `class' if successful, nil otherwise" t nil)

(autoload 'py-backward-def-bol "python-mode" "\
Go to beginning of `def', go to BOL.
If already at beginning, go one `def' backward.
Return beginning of `def' if successful, nil otherwise" t nil)

(autoload 'py-backward-def-or-class-bol "python-mode" "\
Go to beginning of `def-or-class', go to BOL.
If already at beginning, go one `def-or-class' backward.
Return beginning of `def-or-class' if successful, nil otherwise" t nil)

(register-definition-prefixes "python-mode" '("all-mode-setting" "autopair-mode" "flake8" "force-py-shell-name-p-o" "highlight-indent-active" "hs-hide-comments-when-hiding-all" "info-lookup-mode" "ipython" "isympy3" "iypthon" "jython" "pdb-track-stack-from-shell-p" "pep8" "pst-here" "stri" "toggle-force-py-shell-name-p" "turn-o" "virtualenv-"))

;;;***

;;;### (autoloads nil nil ("python-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-mode-autoloads.el ends here
