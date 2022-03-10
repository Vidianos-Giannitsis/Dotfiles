;;; elcord-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elcord" "elcord.el" (0 0 0 0))
;;; Generated autoloads from elcord.el

(defvar elcord-mode nil "\
Non-nil if Elcord mode is enabled.
See the `elcord-mode' command
for a description of this minor mode.")

(custom-autoload 'elcord-mode "elcord" nil)

(autoload 'elcord-mode "elcord" "\
Global minor mode for displaying Rich Presence in Discord.

If called interactively, enable Elcord mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elcord" '("elcord-")))

;;;***

;;;### (autoloads nil nil ("elcord-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elcord-autoloads.el ends here
