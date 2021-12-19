;;; aas-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "aas" "aas.el" (0 0 0 0))
;;; Generated autoloads from aas.el

(autoload 'aas-activate-keymap "aas" "\
Add KEYMAP-SYMBOL to the list of active snippet keymaps.

Return non-nil if that keymap actually exists and was added.
Otherwise return nil.

\(fn KEYMAP-SYMBOL)" nil nil)

(autoload 'aas-mode "aas" "\
Minor mode for dynamically auto-expanding snippets.

If called interactively, enable Aas mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

This does not set any default keymaps. For that use
`aas-activate-for-major-mode' and `aas-activate-keymap'.

\(fn &optional ARG)" t nil)

(put 'aas-global-mode 'globalized-minor-mode t)

(defvar aas-global-mode nil "\
Non-nil if Aas-Global mode is enabled.
See the `aas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `aas-global-mode'.")

(custom-autoload 'aas-global-mode "aas" nil)

(autoload 'aas-global-mode "aas" "\
Toggle Aas mode in all buffers.
With prefix ARG, enable Aas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Aas mode is enabled in all buffers where
`(lambda nil (aas-mode 1) (aas-activate-keymap 'global))' would do it.
See `aas-mode' for more information on Aas mode.

\(fn &optional ARG)" t nil)

(autoload 'aas-activate-for-major-mode "aas" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "aas" '("aas-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aas-autoloads.el ends here
