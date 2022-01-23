;;; maxima-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "maxima" "maxima.el" (0 0 0 0))
;;; Generated autoloads from maxima.el

(autoload 'maxima-mode "maxima" "\


\(fn)" t nil)

(autoload 'maxima-remove-inferior "maxima" "\
Remove the INFERIOR-PROCESS and the process buffer.

\(fn INFERIOR-PROCESS)" nil t)

(autoload 'maxima-start "maxima" "\
Start a maxima process and save the process in INFERIOR-SYMBOL.
The process name is passed in NAME.

\(fn INFERIOR-SYMBOL NAME)" nil t)

(autoload 'maxima "maxima" "\
Run Maxima interactively inside a buffer." t nil)

(put 'global-maxima-minor-mode 'globalized-minor-mode t)

(defvar global-maxima-minor-mode nil "\
Non-nil if Global Maxima minor mode is enabled.
See the `global-maxima-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-maxima-minor-mode'.")

(custom-autoload 'global-maxima-minor-mode "maxima" nil)

(autoload 'global-maxima-minor-mode "maxima" "\
Toggle Maxima minor mode in all buffers.
With prefix ARG, enable Global Maxima minor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Maxima minor mode is enabled in all buffers where
`maxima-minor-mode' would do it.
See `maxima-minor-mode' for more information on Maxima minor mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "maxima" '("maxima-")))

;;;***

;;;### (autoloads nil "maxima-font-lock" "maxima-font-lock.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from maxima-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "maxima-font-lock" '("maxima-font-lock-")))

;;;***

;;;### (autoloads nil nil ("maxima-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; maxima-autoloads.el ends here
