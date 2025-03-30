;;; ngerman.el --- Setup AUCTeX for editing German text.  -*- lexical-binding: t; -*-

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Cater for some specialities of `(n)german.sty', e.g. special quote
;; and hyphen strings or that `"' makes the following letter an
;; umlaut.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-quotes
                  "font-latex"
                  (quotes))

(declare-function font-latex-add-to-syntax-alist
                  "font-latex"
                  (list))

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook
 "ngerman"
 (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language '("ngerman" "\"`" "\"'" t)))
   (setq LaTeX-babel-hyphen-language "ngerman")
   ;; Fontification
   (when (and (eq TeX-install-font-lock 'font-latex-setup)
              (featurep 'font-latex))
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german))
     ;; Prevent "| from leading to color bleed.
     (font-latex-add-to-syntax-alist (list (cons ?\" "\\"))))
   (run-hooks 'TeX-language-de-hook))
 TeX-dialect)

;;; ngerman.el ends here
