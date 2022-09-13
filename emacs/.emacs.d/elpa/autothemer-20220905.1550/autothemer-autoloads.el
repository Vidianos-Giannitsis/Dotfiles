;;; autothemer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "autothemer" "autothemer.el" (0 0 0 0))
;;; Generated autoloads from autothemer.el

(autoload 'autothemer-deftheme "autothemer" "\
Define a theme NAME with description DESCRIPTION.
A color PALETTE can be used to define `let*'-like
bindings within both the REDUCED-SPECS and the BODY.

\(fn NAME DESCRIPTION PALETTE REDUCED-SPECS &rest BODY)" nil t)

(autoload 'autothemer-generate-templates-filtered "autothemer" "\
Autogenerate customizations for unthemed faces matching REGEXP.

Calls `autothemer-generate-templates' after user provides REGEXP interactively.

\(fn REGEXP)" t nil)

(autoload 'autothemer-generate-templates "autothemer" "\
Autogenerate customizations for unthemed faces (optionally by REGEXP).

Generate customizations that approximate current face definitions using the
nearest colors in the color palette of `autothemer-current-theme'.

An error is shown when no current theme is available.

\(fn &optional REGEXP)" t nil)

(register-definition-prefixes "autothemer" '("autothemer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autothemer-autoloads.el ends here
