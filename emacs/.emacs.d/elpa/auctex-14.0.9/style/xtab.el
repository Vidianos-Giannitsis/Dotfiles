;;; xtab.el --- AUCTeX style for `xtab.sty' (v2.3f)  -*- lexical-binding: t; -*-

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

;; This file adds support for `xtab.sty' (v2.3f) from 2011/07/31.
;; `xtab.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(TeX-add-style-hook
 "xtab"
 (lambda ()

   ;; Run the style hook for supertabular.el:
   (TeX-run-style-hooks "supertabular")

   (TeX-add-symbols
    '("tablelasthead" t)
    '("notablelasthead" 0)
    '("xentrystretch" "Fraction"))

   ;; Add the environments provided by the package:
   (LaTeX-add-environments
    '("xtabular"    LaTeX-env-array)
    '("xtabular*"   LaTeX-env-array)
    '("mpxtabular"  LaTeX-env-array)
    '("mpxtabular*" LaTeX-env-array))

   ;; Append the environments to `LaTeX-item-list':
   (dolist (env '("xtabular" "mpxtabular"))
     (add-to-list 'LaTeX-item-list (cons env 'LaTeX-item-array) t)
     (add-to-list 'LaTeX-item-list (cons (concat env "*") 'LaTeX-item-array) t))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("notablelasthead" "")
                                ("xentrystretch"   "{"))
                              'function)
     (font-latex-add-keywords '(("tablelasthead"   "{"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-xtab-package-options
  '("errorshow" "pageshow" "debugshow")
  "Package options for the xtab package.")

;;; xtab.el ends here
