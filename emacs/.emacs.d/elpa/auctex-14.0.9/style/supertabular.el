;;; supertabular.el --- AUCTeX style for `supertabular.sty' (v4.2c)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-07-21
;; Keywords: tex

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

;; This file adds support for `supertabular.sty' (v4.2c) from 2024/07/20.
;; `supertabular.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(defun LaTeX-supertabular-insert-caption (optional &optional prompt)
  "Insert the arguments of caption macros provided by supertabular package."
  (let* ((caption (TeX-read-string
                   (TeX-argument-prompt optional prompt "Caption")))
         (short-caption
          (if (>= (length caption) LaTeX-short-caption-prompt-length)
              (TeX-read-string
               (TeX-argument-prompt t nil "Short caption"))
            "")))
    (TeX-argument-insert short-caption t)
    (TeX-argument-insert caption optional)
    (when auto-fill-function (LaTeX-fill-paragraph))))

(TeX-add-style-hook
 "supertabular"
 (lambda ()
   (TeX-add-symbols
    '("tablefirsthead" t)
    '("tablehead" t)

    '("tabletail" t)
    '("tablelasttail" t)

    '("topcaption" LaTeX-supertabular-insert-caption)
    '("bottomcaption" LaTeX-supertabular-insert-caption)
    '("tablecaption" LaTeX-supertabular-insert-caption)

    '("shrinkheight" (TeX-arg-length "Max. height")))

   ;; Add the environments provided by the package:
   (LaTeX-add-environments
    '("supertabular"    LaTeX-env-array)
    '("supertabular*"   LaTeX-env-tabular*)
    '("mpsupertabular"  LaTeX-env-array)
    '("mpsupertabular*" LaTeX-env-tabular*))

   ;; Append the environments to `LaTeX-item-list':
   (add-to-list 'LaTeX-item-list '("supertabular" . LaTeX-item-array) t)
   (add-to-list 'LaTeX-item-list '("supertabular*" . LaTeX-item-tabular*) t)
   (add-to-list 'LaTeX-item-list '("mpsupertabular" . LaTeX-item-array) t)
   (add-to-list 'LaTeX-item-list '("mpsupertabular*" . LaTeX-item-tabular*) t)

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("shrinkheight"   "{"))
                              'function)
     (font-latex-add-keywords '(("tablefirsthead" "{")
                                ("tablehead"      "{")
                                ("tabletail"      "{")
                                ("tablelasttail"  "{")
                                ("topcaption"     "[{")
                                ("bottomcaption"  "[{")
                                ("tablecaption"   "[{"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-supertabular-package-options
  '("errorshow" "pageshow" "debugshow" "estimate" "calculate")
  "Package options for the supertabular package.")

;;; supertabular.el ends here
