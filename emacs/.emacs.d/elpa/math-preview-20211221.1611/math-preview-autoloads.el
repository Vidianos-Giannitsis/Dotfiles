;;; math-preview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "math-preview" "math-preview.el" (0 0 0 0))
;;; Generated autoloads from math-preview.el

(autoload 'math-preview-start-process "math-preview" "\
Start math-preview process." t nil)

(autoload 'math-preview-stop-process "math-preview" "\
Stop math-preview process." t nil)

(autoload 'math-preview-region "math-preview" "\
Preview equations in region between `BEG` and `END`.

\(fn BEG END)" t nil)

(autoload 'math-preview-all "math-preview" "\
Preview equations in buffer." t nil)

(autoload 'math-preview-at-point "math-preview" "\
Preview equations at point." t nil)

(autoload 'math-preview-clear-region "math-preview" "\
Remove all preview overlays in region between `BEG` and `END`.

\(fn BEG END)" t nil)

(autoload 'math-preview-clear-at-point "math-preview" "\
Remove all preview overlays." t nil)

(autoload 'math-preview-clear-all "math-preview" "\
Remove all preview overlays." t nil)

(autoload 'math-preview-increment-scale "math-preview" "\
Increment image size.
Scale is changed by `N` times `math-preview-scale-increment`

\(fn N)" t nil)

(autoload 'math-preview-decrement-scale "math-preview" "\
Decrement image size.
Scale is changed by `N` times `math-preview-scale-increment`

\(fn N)" t nil)

(autoload 'math-preview-copy-svg "math-preview" "\
Copy SVG image to clipboard." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "math-preview" '("math-preview-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; math-preview-autoloads.el ends here
