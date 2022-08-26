;;; ac-cider-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-cider" "ac-cider.el" (0 0 0 0))
;;; Generated autoloads from ac-cider.el

(defface ac-cider-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for nrepl candidates." :group 'auto-complete)

(defface ac-cider-selection-face '((t (:inherit ac-selection-face))) "\
Face for the nrepl selected candidate." :group 'auto-complete)

(defconst ac-cider-source-defaults '((available . ac-cider-available-p) (candidate-face . ac-cider-candidate-face) (selection-face . ac-cider-selection-face) (prefix . cider-completion-symbol-start-pos) (match . ac-cider-match-fuzzy) (document . ac-cider-documentation) (cache)) "\
Defaults common to the various completion sources.")

(defvar ac-source-cider-everything (append '((candidates . ac-cider-candidates-everything) (symbol . "v")) ac-cider-source-defaults) "\
Auto-complete source for CIDER buffers.")

(autoload 'ac-cider-setup "ac-cider" "\
Add the CIDER completion source to the front of `ac-sources'.
This affects only the current buffer." t nil)

(autoload 'ac-cider-popup-doc "ac-cider" "\
A popup alternative to `cider-doc'." t nil)

(register-definition-prefixes "ac-cider" '("ac-cider-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-cider-autoloads.el ends here
