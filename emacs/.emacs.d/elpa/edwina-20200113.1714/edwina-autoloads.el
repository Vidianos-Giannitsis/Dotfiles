;;; edwina-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edwin" "edwin.el" (0 0 0 0))
;;; Generated autoloads from edwin.el

(defvaralias 'edwin-mode 'edwina-mode)

(defalias #'edwin-mode #'edwina-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edwin" '("edwin-")))

;;;***

;;;### (autoloads nil "edwina" "edwina.el" (0 0 0 0))
;;; Generated autoloads from edwina.el

(defvar edwina-mode nil "\
Non-nil if Edwina mode is enabled.
See the `edwina-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `edwina-mode'.")

(custom-autoload 'edwina-mode "edwina" nil)

(autoload 'edwina-mode "edwina" "\
Toggle Edwina mode on or off.
With a prefix argument ARG, enable Edwina mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Edwina mode is a global minor mode that provides dwm-like dynamic
window management for Emacs windows.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edwina" '("edwina-")))

;;;***

;;;### (autoloads nil nil ("edwina-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edwina-autoloads.el ends here
