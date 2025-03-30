;;; stocksize.el - AUCTeX style for `stocksize.sty' (v1.0.3) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-11-26
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

;; This file adds support for `stocksize.sty' (v1.0.3) from 2024/11/23.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(TeX-add-style-hook
 "stocksize"
 (lambda ()

   ;; Run the style hook `geometry.sty':
   (TeX-run-style-hooks "geometry")

   (TeX-add-symbols
    '("newstocksize"
      (TeX-arg-key-val (lambda ()
                         (append '(("keepmargins" ("true" "false")))
                                 LaTeX-geometry-always-key-val-options))))
    '("restorestocksize" 0))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newstocksize"     "{")
                                ("restorestocksize" ""))
                              'function)))
 TeX-dialect)

(defvar LaTeX-stocksize-package-options nil
  "Package options for the stocksize package.")

;;; stocksize.el ends here
