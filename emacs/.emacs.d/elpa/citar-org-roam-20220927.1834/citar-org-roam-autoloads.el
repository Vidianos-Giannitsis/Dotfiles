;;; citar-org-roam-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "citar-org-roam" "citar-org-roam.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from citar-org-roam.el

(defvar citar-org-roam-mode nil "\
Non-nil if citar-org-roam mode is enabled.
See the `citar-org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `citar-org-roam-mode'.")

(custom-autoload 'citar-org-roam-mode "citar-org-roam" nil)

(autoload 'citar-org-roam-mode "citar-org-roam" "\
Toggle `citar-org-roam-mode'.

This is a minor mode.  If called interactively, toggle the
`citar-org-roam mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='citar-org-roam-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "citar-org-roam" '("citar-org-roam-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; citar-org-roam-autoloads.el ends here
