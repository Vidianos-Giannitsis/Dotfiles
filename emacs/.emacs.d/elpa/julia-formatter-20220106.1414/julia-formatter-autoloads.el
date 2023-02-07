;;; julia-formatter-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-formatter" "julia-formatter.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from julia-formatter.el

(autoload 'julia-formatter-format-region "julia-formatter" "\
Format region delimited by BEGIN and END  using JuliaFormatter.jl.

Region must have self-contained code.  If not, the region won't be formatted and
will remain as-is.

\(fn BEGIN END)" nil nil)

(autoload 'julia-formatter-beginning-of-defun "julia-formatter" "\
Get beginning of surrounding debufn from `julia-formatter--defun-range'.

Move to the ARG -th beginning of defun.

\(fn &optional (ARG 1))" nil nil)

(autoload 'julia-formatter-end-of-defun "julia-formatter" "\
Get beginning of surrounding debufn from `julia-formatter--defun-range'.

See `end-of-defun-function' to understand values of ARG.

\(fn &optional (ARG 1))" nil nil)

(autoload 'julia-formatter-format-buffer "julia-formatter" "\
Format the whole buffer" nil nil)

(autoload 'julia-formatter-mode "julia-formatter" "\
Setup buffer for formatting code using indenting functions.

This is a minor mode.  If called interactively, toggle the
`Julia-Formatter mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `julia-formatter-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

See documentation on `indent-region-function' for different ways you can indent
current buffer (by line, by region, whole buffer ...)

When `julia-formatter-setup-for-save' is non-nil, will format buffer before
saving.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "julia-formatter" '("julia-formatter-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-formatter-autoloads.el ends here
