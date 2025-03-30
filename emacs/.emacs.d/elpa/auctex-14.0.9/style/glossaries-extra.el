;;; glossaries-extra.el - AUCTeX style for `glossaries-extra.sty' (v1.53) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-11-11
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

;; This file adds incomplete support for `glossaries-extra.sty' (v1.53)
;; from 2023/09/29.  `glossaries-extra.sty' is huge, and this style
;; covers only part of the macros described in
;; `glossaries-extra-manual.pdf'.

;; FIXME: Please make me more sophisticated!

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(defvar LaTeX-glossaries-package-options-list)
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(TeX-add-style-hook
 "glossaries-extra"
 (lambda ()

   ;; Run the style hook `glossaries.sty':
   (TeX-run-style-hooks "glossaries")

   (TeX-add-symbols
    '("longnewglossaryentry*" LaTeX-glossaries-insert-newentry
      (TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s)
      t)

    '("newabbreviation"
      [TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s]
      LaTeX-glossaries-insert-newentry
      "Short form" "Long form"))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newabbreviation" "[{{{"))
                              'function)))
 TeX-dialect)


(defvar LaTeX-glossaries-extra-package-options-list
  '(("undefaction" ("error" "warn"))
    ("docdef" ("false" "restricted" "atom" "true"))
    ("stylemods")
    ("indexcrossrefs" ("true" "false"))
    ("autoseeindex" ("true" "false"))
    ("record" ("off" "only" "nameref" "hybrid"))
    ("equation" ("true" "false"))
    ("floats" ("true" "false"))
    ("indexcounter")
    ;; 2.7. Acronym and Abbreviation Options
    ("abbreviations")
    ;; 2.9. Other Options
    ("accsupp")
    ("prefix")
    ("nomissingglstext" ("true" "false")))
  "Package options for the glossaries-extra package.")

(defun LaTeX-glossaries-extra-package-options ()
  "Read the glossaries-extra package options from the user."
  (TeX-load-style "glossaries")
  (TeX-read-key-val t (append
                       LaTeX-glossaries-package-options-list
                       LaTeX-glossaries-extra-package-options-list)))

;;; glossaries-extra.el ends here
