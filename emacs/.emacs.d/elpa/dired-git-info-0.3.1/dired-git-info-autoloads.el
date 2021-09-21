;;; dired-git-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-git-info" "dired-git-info.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dired-git-info.el

(autoload 'dired-git-info-mode "dired-git-info" "\
Toggle git message info in current dired buffer.

If called interactively, enable Dired-Git-Info mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'dired-git-info-auto-enable "dired-git-info" "\
Enable `dired-git-info-mode' if current dired buffer is in a git repo.

Add this function to `dired-after-readin-hook' to enable the mode
automatically inside git repos." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-git-info" '("dgi-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-git-info-autoloads.el ends here
