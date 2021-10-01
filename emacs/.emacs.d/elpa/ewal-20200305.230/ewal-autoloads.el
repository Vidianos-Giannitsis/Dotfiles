;;; ewal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ewal" "ewal.el" (0 0 0 0))
;;; Generated autoloads from ewal.el

(autoload 'ewal-load-colors "ewal" "\
Read JSON as the most complete of the cached wal files.
COLOR-NAMES will be associated with the first 8 colors of the
cached wal colors.  COLOR-NAMES are meant to be used in
conjunction with `ewal-ansi-color-name-symbols'.  \"Special\" wal
colors such as \"background\", \"foreground\", and \"cursor\",
tend to (but do not always) correspond to the remaining colors
generated by wal. Add those special colors to the returned
alist. Return nil on failure.

\(fn &optional JSON COLOR-NAMES)" nil nil)

(autoload 'ewal-load-color "ewal" "\
Same as `ewal-get-color' but call `ewal-load-ewal-colors' first.
Pass COLOR, SHADE, and SHADE-PERCENT-DIFFERENCE to
`ewal-get-color'.  Meant to be called from user config.

\(fn COLOR &optional SHADE SHADE-PERCENT-DIFFERENCE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ewal" '("ewal-")))

;;;***

;;;### (autoloads nil nil ("ewal-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ewal-autoloads.el ends here