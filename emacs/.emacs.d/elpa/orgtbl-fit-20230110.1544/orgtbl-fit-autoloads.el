;;; orgtbl-fit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgtbl-fit" "orgtbl-fit.el" (0 0 0 0))
;;; Generated autoloads from orgtbl-fit.el

(autoload 'orgtbl-fit "orgtbl-fit" "\
Add a column to an Org Mode table which fits the current column.
A user-supplied MODEL is applied to columns mentioned in the MODEL
in order to match as close as possible the column where the cursor is on.
A new column is added, with values computed with the MODEL.
Usually, no exact fit can be found.  Another column is added showing
differences between actual values and fitted values.
A default linear-with-constant MODEL is proposed as a default.
It mentions only columns containing numerical values.

\(fn &optional MODEL)" t nil)

(register-definition-prefixes "orgtbl-fit" '("orgtbl-fit--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgtbl-fit-autoloads.el ends here
