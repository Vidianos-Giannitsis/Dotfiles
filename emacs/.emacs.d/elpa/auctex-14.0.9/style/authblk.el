;;; authblk.el --- AUCTeX style for `authblk.sty' (v1.3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-07-05
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

;; This file adds support for `authblk.sty' (v1.3) from 2001/02/27.
;; `authblk.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "authblk"
 (lambda ()
   (TeX-add-symbols
    '("author" ["Footnote marker"] LaTeX-arg-author)
    '("affil"  ["Footnote marker"] "Affiliation")
    '("authorcr" 0)
    "Authfont"
    "Affilfont"
    "Authsep"
    "Authand"
    "Authands")

   (LaTeX-add-counters "Maxaffil")
   (LaTeX-add-lengths "affilsep")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("author" "[{")
                                ("affil"  "[{"))
                              'textual)
     (font-latex-add-keywords '("authorcr")
                              'warning)))
 TeX-dialect)

(defvar LaTeX-authblk-package-options
  '("blocks" "noblocks" "max2" "max3" "max4" "max5" "max6"
    "auth-sc" "auth-sc-lg" "auth-lg" "affil-sl" "affil-it" "german")
  "Package options for the authblk package.")

;;; authblk.el ends here
