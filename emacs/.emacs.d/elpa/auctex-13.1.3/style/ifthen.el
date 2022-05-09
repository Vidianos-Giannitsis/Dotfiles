;;; ifthen.el --- AUCTeX style for `ifthen.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-03-16
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `ifthen.sty' v1.1c, dated 2020/11/24.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-arg-ifthen-test (optional &optional prompt)
  "Query and insert skeleton for a test in ifthen macros."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Test")
    '("<" "=" ">"
      "\\isodd{}"
      "\\isundefined{}"
      "\\equal{}{}"
      "\\lengthtest{<}"
      "\\lengthtest{=}"
      "\\lengthtest{>}"
      "\\boolean{}"))
   optional))

(TeX-add-style-hook
 "ifthen"
 (lambda ()
   (TeX-add-symbols
    '("ifthenelse" LaTeX-arg-ifthen-test t nil)
    '("whiledo"    LaTeX-arg-ifthen-test t)
    "AND"
    "OR"
    "NOT"
    '("newboolean" "Name")
    '("provideboolean" "Name")
    '("setboolean" "Name" (TeX-arg-eval completing-read
                                        (TeX-argument-prompt nil nil "Value")
                                        '("true" "false"))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     ;; Only fontify macros which will be used top-level:
     (font-latex-add-keywords '(("newboolean"     "{")
                                ("provideboolean" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-ifthen-package-options nil
  "Package options for the ifthen package.")

;;; ifthen.el ends here
