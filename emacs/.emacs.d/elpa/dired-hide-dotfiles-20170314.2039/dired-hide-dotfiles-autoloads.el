;;; dired-hide-dotfiles-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-hide-dotfiles" "dired-hide-dotfiles.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from dired-hide-dotfiles.el

(autoload 'dired-hide-dotfiles-mode "dired-hide-dotfiles" "\
Toggle `dired-hide-dotfiles-mode'

If called interactively, enable Dired-Hide-Dotfiles mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'dired-hide-dotfiles--hide "dired-hide-dotfiles" "\
Hide all dot-files in the current `dired' buffer." nil nil)

(add-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-hide-dotfiles-autoloads.el ends here
