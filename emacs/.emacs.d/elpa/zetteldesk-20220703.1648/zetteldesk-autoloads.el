;;; zetteldesk-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zetteldesk" "zetteldesk.el" (0 0 0 0))
;;; Generated autoloads from zetteldesk.el

(defvar zetteldesk-mode nil "\
Non-nil if zetteldesk mode is enabled.
See the `zetteldesk-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `zetteldesk-mode'.")

(custom-autoload 'zetteldesk-mode "zetteldesk" nil)

(autoload 'zetteldesk-mode "zetteldesk" "\
Toggles the global `zetteldesk-mode'.

This is a minor mode.  If called interactively, toggle the
`zetteldesk mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='zetteldesk-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When turned on, this mode initialises the *zetteldesk-scratch*
buffer, a useful part of the whole zetteldesk workflow.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "zetteldesk" '("zetteldesk-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zetteldesk-autoloads.el ends here
