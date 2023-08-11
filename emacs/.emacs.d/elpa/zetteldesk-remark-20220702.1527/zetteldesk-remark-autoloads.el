;;; zetteldesk-remark-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zetteldesk-remark" "zetteldesk-remark.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zetteldesk-remark.el

(defvar zetteldesk-remark-mode nil "\
Non-nil if zetteldesk-remark mode is enabled.
See the `zetteldesk-remark-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `zetteldesk-remark-mode'.")

(custom-autoload 'zetteldesk-remark-mode "zetteldesk-remark" nil)

(autoload 'zetteldesk-remark-mode "zetteldesk-remark" "\
Toggle the `zetteldesk-remark-mode'.

This is a minor mode.  If called interactively, toggle the
`zetteldesk-remark mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='zetteldesk-remark-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode initialises the value of `zetteldesk-remark-title', an
important variable for using org-remark in buffers not associated
to a file.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "zetteldesk-remark" '("zetteldesk-remark-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zetteldesk-remark-autoloads.el ends here
